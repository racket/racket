#lang racket/base
(require (for-syntax racket/base)
         racket/private/vector-wraps
         racket/match
         racket/dict
         racket/contract/base
         racket/fixnum
         racket/unsafe/ops
         racket/serialize)

(define bits-in-a-word 8)
(define bits-in-a-word-log2 3)

(define largest-word
  (- (expt 2 bits-in-a-word) 1))
  
(define (word-index n)
  (arithmetic-shift n (- bits-in-a-word-log2)))
(define (bit-index n)
  (bitwise-and n (sub1 bits-in-a-word)))

(define (make-bit-vector size [fill #f])
  (define-values (q r) (quotient/remainder size bits-in-a-word))
  (define word-size (+ q (if (zero? r) 0 1)))
  (define words (make-bytes word-size (if fill largest-word 0)))  
  (when (and fill (not (zero? r))) 
    (bytes-set! words q (- (expt 2 r) 1)))
  (bit-vector words size))

(define bit-vector*
  (let ([bit-vector
         (lambda init-bits
           (list->bit-vector init-bits))])
    bit-vector))

(define not-given (gensym))

(define (bit-vector-ref bv n [default not-given])
  ;; use "natural?" here to be compatible with the previously used contract
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'bit-vector-ref "natural?" n))
  (match bv
    [(struct bit-vector (words size))
     (cond
       [(< n size) (unsafe-words-ref words n)]
       [else
        (cond
          [(eq? default not-given)
           (raise-range-error 'bit-vector-ref
                              "bit-vector"
                              "" n bv 0 (sub1 size))]
          [(procedure? default) (default)]
          [else default])])]
    [_ (raise-argument-error 'bit-vector-ref "bit-vector?" bv)]))

(define (unsafe-bit-vector-ref bv n)
  (unsafe-words-ref (bit-vector-words bv) n))

;; precondition:
;; - words is a bytestring
;; - n is a fixnum less than words's length (as a bitvector)
(define (unsafe-words-ref words n)
  (define wi (word-index n))
  (define bi (bit-index n))
  (define word (unsafe-bytes-ref words wi))
  (unsafe-bitwise-bit-set? word bi))

(define (bit-vector-iterate-first bv)
  (if (zero? (bit-vector-size bv)) #f 0))

(define (bit-vector-iterate-next bv pos)
  (if (>= (+ pos 1) (bit-vector-size bv))
      #f
      (+ pos 1)))

(define (bit-vector-iterate-key bv key)
  key)

(define (bit-vector-iterate-value bv key)
  (bit-vector-ref bv key))

(define (unsafe-bitwise-bit-set? n m)
  (not (eq? 0 (unsafe-fxand n (unsafe-fxlshift 1 m)))))

;; precondition:
;; - words is a bytestring
;; - n is a fixnum less than words's length (as a bitvector)
;; - b is a boolean
(define (unsafe-words-set! words n b)
  (define wi (word-index n))
  (define bi (bit-index n))
  (define word (unsafe-bytes-ref words wi))
  (define bit  (unsafe-bitwise-bit-set? word bi))
  (unless (eq? bit b)
    (define new-word (unsafe-fxxor word (unsafe-fxlshift 1 bi)))
    (unsafe-bytes-set! words wi new-word)))

(define (bit-vector-set! bv n b)
  ;; use "natural?" here to be compatible with the previously used contract
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'bit-vector-set! "natural?" n))
  (unless (boolean? b)
    (raise-argument-error 'bit-vector-set! "boolean?" b))
  (match bv
    [(struct bit-vector (words size))
     (cond
       [(< n size) (unsafe-words-set! words n b)]
       [else (raise-range-error 'bit-vector-set!
                                "bit-vector"
                                "" n bv 0 (sub1 size))])]
    [_ (raise-argument-error 'bit-vector-set! "bit-vector?" bv)]))

(define (bit-vector-length bv)
  (bit-vector-size bv))

(define bit-vector-copy*
  (let ([bit-vector-copy
         (case-lambda
          [(bv)
           (bit-vector (bytes-copy (bit-vector-words bv))
                       (bit-vector-size bv))]
          [(bv start)
           (bit-vector-copy bv start)]
          [(bv start end)
           (bit-vector-copy bv start end)])])
    bit-vector-copy))

(define (bit-vector-popcount bv)
  (for/sum ([b (in-bytes (bit-vector-words bv))])
    (fxpopcount16 b)))

(define (bit-vector->list bv)
  (define len (bit-vector-size bv))
  (define words (bit-vector-words bv))
  (let loop ([i 0])
    (cond [(< i len)
           (cons (unsafe-words-ref words i)
                 (loop (add1 i)))]
          [else null])))

(define (list->bit-vector init-bits)
  (define len (length init-bits))
  (define bv (make-bit-vector len))
  (define words (bit-vector-words bv))
  (for ([i (in-range len)]
        [b (in-list init-bits)])
    (unsafe-words-set! words i b))
  bv)

(define (bit-vector->string bv)
  (let* ([l (bit-vector-size bv)]
         [words (bit-vector-words bv)]
         [s (make-string l)])
    (for ([i (in-range l)])
      (string-set! s i (if (unsafe-words-ref words i) #\1 #\0)))
    s))

(define (string->bit-vector s)
  (let* ([bv (make-bit-vector (string-length s) #f)]
         [words (bit-vector-words bv)])
    (for ([i (in-range (string-length s))])
      (when (eqv? (string-ref s i) #\1)
        (unsafe-words-set! words i #t)))
    bv))


(define-vector-wraps "bit-vector"
  "boolean?" boolean?
  bit-vector? bit-vector-length bit-vector-ref bit-vector-set! make-bit-vector
  unsafe-bit-vector-ref bit-vector-set! bit-vector-length
  in-bit-vector*
  in-bit-vector
  for/bit-vector
  for*/bit-vector
  bit-vector-copy
  #f
  check-bitvector)

;; A bit vector is represented as bytes.
(serializable-struct bit-vector (words size)
  ; words     is the bytes of words
  ; size      is the number of bits in bitvector
  #:guard ;; needed because deserialization doesn't go through contracts
  (lambda (words size _name)
    (unless (bytes? words)
      (raise-argument-error 'bit-vector "bytes?" words))
    (unless (exact-nonnegative-integer? size)
      (raise-argument-error 'bit-vector "exact-nonnegative-integer?" size))
    (let-values ([(q r) (quotient/remainder size bits-in-a-word)])
      (unless (and (= (bytes-length words) (+ q (if (zero? r) 0 1)))
                   ;; make sure "unreachable" bits are unset
                   (or (zero? r) (< (bytes-ref words q) (expt 2 r))))
        (error 'bit-vector "bit vector data contains wrong number of bits")))
    (values words size))
  #:property prop:dict/contract
  (list (vector-immutable bit-vector-ref
                          bit-vector-set!
                          #f ;; set
                          #f ;; remove!
                          #f ;; remove
                          bit-vector-length
                          bit-vector-iterate-first
                          bit-vector-iterate-next
                          bit-vector-iterate-key
                          bit-vector-iterate-value)
        (vector-immutable exact-nonnegative-integer?
                          boolean?
                          exact-nonnegative-integer?
                          #f #f #f))
  #:methods gen:equal+hash
  [(define (equal-proc x y recursive-equal?)
     (let ([vx (bit-vector-words x)]
           [vy (bit-vector-words y)]
           [nx (bit-vector-size x)]
           [ny (bit-vector-size y)])
       (and (= nx ny)
            (or (zero? nx) ;zero-length bit-vectors are equal
                (let ([last-index (sub1 (bytes-length vx))])
                  (and
                   ;; Check all but last byte.
                   ;; These use all bits, therefore simple eq?.
                   (for/and ([index (in-range (sub1 last-index))])
                     (eq? (bytes-ref vx index)
                          (bytes-ref vy index)))
                   ;; Check the used bits of the last byte.
                   (let ([used-bits (min 8 (remainder nx 256))])
                     (eq? (bitwise-bit-field (bytes-ref vx last-index)
                                             0
                                             used-bits)
                          (bitwise-bit-field (bytes-ref vy last-index)
                                             0
                                             used-bits)))))))))
   (define (hash-code x hc)
     (let ([v (bit-vector-words x)]
           [n (bit-vector-size x)])
       (bitwise-xor
        (hc n)
        (for/fold ([h 1]) ([i (in-range (bytes-length v))])
          (bitwise-xor h (hc (bytes-ref v i)))))))
   (define hash-proc  hash-code)
   (define hash2-proc hash-code)]
  #:property prop:sequence in-bit-vector)

(provide bit-vector-ref
         bit-vector-set!)

(provide/contract
 [bit-vector?
  (-> any/c any)]
 [rename bit-vector* bit-vector
         (->* () () #:rest (listof boolean?) bit-vector?)]
 [make-bit-vector
  (->* (exact-nonnegative-integer?) (boolean?)   bit-vector?)]
 [bit-vector-length
  (-> bit-vector? any)]
 [bit-vector-popcount
  (-> bit-vector? any)]
 (rename bit-vector-copy*
         bit-vector-copy
         (->* [bit-vector?] [exact-nonnegative-integer? exact-nonnegative-integer?] bit-vector?))
 [bit-vector->list
  (-> bit-vector? (listof boolean?))]
 [list->bit-vector
  (-> (listof boolean?) bit-vector?)]
 [bit-vector->string
  (-> bit-vector? string?)]
 [string->bit-vector
  (-> (and/c string? #rx"^[01]*$") bit-vector?)])

(provide in-bit-vector for/bit-vector for*/bit-vector)
