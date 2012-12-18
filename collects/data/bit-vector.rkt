#lang racket/base
(require (for-syntax racket/base)
         (for-syntax "private/count-bits-in-fixnum.rkt")
         racket/private/vector-wraps
         racket/match
         racket/dict
         racket/contract/base
         racket/fixnum
         racket/unsafe/ops
         racket/serialize
         "private/count-bits-in-fixnum.rkt")

(define bits-in-a-word 8)

(define largest-word
  (- (expt 2 bits-in-a-word) 1))

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
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'bit-vector-ref "exact-nonnegative-integer?" n))
  (cond [(< n (bit-vector-size bv))
         (unsafe-bit-vector-ref bv n)]
        [else
         (cond [(eq? default not-given)
                (raise-range-error 'bit-vector-ref
                                   "bit-vector"
                                   "" n bv 0 (sub1 (bit-vector-size bv)))]
               [(procedure? default)
                (default)]
               [else default])]))

(define (unsafe-bit-vector-ref bv n)
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  (match bv
    [(struct bit-vector (words size))
     (define word (bytes-ref words wi))
     (define bit  (bitwise-bit-set? word bi))
     bit]))

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

(define (bit-vector-set! bv n b)
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  (match bv
    [(struct bit-vector (words size))
     (define word (bytes-ref words wi))
     (define bit  (bitwise-bit-set? word bi))
     (unless (eq? bit b)
       (bytes-set! words wi (bitwise-xor word (expt 2 bi))))]))

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

(define popcount-table
  (let ()
    (define-syntax (make-table stx)
      (with-syntax ([(elt ...)
                     (for/list ([i (in-range 256)])
                       (fxpopcount i))])
        ;; Literal immutable vector allocated once (?)
        #'(quote #(elt ...))))
    (make-table)))

(define (bit-vector-popcount bv)
  (for/sum ([b (in-bytes (bit-vector-words bv))])
    #| (unsafe-fxpopcount* b 8) |#
    (unsafe-vector-ref popcount-table b)))

(define (bit-vector->list bv)
  (define len (bit-vector-size bv))
  (let loop ([i 0])
    (cond [(< i len)
           (cons (unsafe-bit-vector-ref bv i)
                 (loop (add1 i)))]
          [else null])))

(define (list->bit-vector init-bits)
  (define len (length init-bits))
  (define bv (make-bit-vector len))
  (for ([i (in-range len)]
        [b (in-list init-bits)])
    (bit-vector-set! bv i b))
  bv)

(define (bit-vector->string bv)
  (let* ([l (bit-vector-size bv)]
         [s (make-string l)])
    (for ([i (in-range l)])
      (string-set! s i (if (unsafe-bit-vector-ref bv i) #\1 #\0)))
    s))

(define (string->bit-vector s)
  (let* ([bv (make-bit-vector (string-length s) #f)])
    (for ([i (in-range (string-length s))])
      (when (eqv? (string-ref s i) #\1)
        (bit-vector-set! bv i #t)))
    bv))


(define-vector-wraps "bit-vector"
  bit-vector? bit-vector-length bit-vector-ref bit-vector-set! make-bit-vector
  unsafe-bit-vector-ref bit-vector-set! bit-vector-length
  in-bit-vector*
  in-bit-vector
  for/bit-vector
  for*/bit-vector
  bit-vector-copy
  #f)

; A bit vector is represented as a vector of words.
; Each word contains 30 or 62 bits depending on the size of a fixnum.
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
            (for/and ([index (in-range (- (bytes-length vx) 1))])
              (eq? (bytes-ref vx index)
                   (bytes-ref vy index)))
            ; TODO: check last word
            )))
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

(provide/contract
 [bit-vector?
  (-> any/c any)]
 [rename bit-vector* bit-vector
         (->* () () #:rest (listof boolean?) bit-vector?)]
 [make-bit-vector
  (->* (exact-nonnegative-integer?) (boolean?)   bit-vector?)]
 [bit-vector-ref
  (->* (bit-vector? exact-nonnegative-integer?) (any/c) any)]
 [bit-vector-set!
  (-> bit-vector? exact-nonnegative-integer? boolean? any)] 
 [bit-vector-length
  (-> bit-vector? any)]
 [bit-vector-popcount
  (-> bit-vector? any)]
 (rename bit-vector-copy*
         bit-vector-copy
         (-> bit-vector? bit-vector?))
 [bit-vector->list
  (-> bit-vector? (listof boolean?))]
 [list->bit-vector
  (-> (listof boolean?) bit-vector?)]
 [bit-vector->string
  (-> bit-vector? string?)]
 [string->bit-vector
  (-> (and/c string? #rx"^[01]*$") bit-vector?)])

(provide in-bit-vector for/bit-vector for*/bit-vector)
