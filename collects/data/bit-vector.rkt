#lang racket/base
(require (for-syntax racket/base)
         racket/private/vector-wraps
         racket/match
         racket/dict         
         racket/contract/base
         racket/vector
         racket/unsafe/ops)

(define bits-in-a-word
  (if (fixnum? (expt 2 61)) 
      ; 32 or 64-bit fixnums?
      62 30))

(define largest-fixnum
  (- (expt 2 bits-in-a-word) 1))

(define ((bad-index-error who index))
  (raise-mismatch-error who "index out of range: " index))

(define (make-bit-vector size [fill #f])
  (define word-size (add1 (quotient size bits-in-a-word)))
  (define words (make-vector word-size (if fill largest-fixnum 0)))
  (bit-vector words size word-size))

(define (bit-vector* . init-bits)
  (define bv (make-bit-vector (length init-bits)))
  (for ([i (in-naturals)]
        [b (in-list init-bits)])
    (bit-vector-set! bv i b))
  bv)

(define (bit-vector-ref bv n 
                        [default (bad-index-error 'bit-vector-ref n)])
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'bit-vector-ref "exact nonnegative integer" n))
  (cond
    [(>= n (bit-vector-size bv))
     (if (procedure? default)
         (default)
         default)]
    [else
     (unsafe-bit-vector-ref bv n)]))

(define (unsafe-bit-vector-ref bv n)
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  (match bv
    [(struct bit-vector (words size word-size))
     (define word (vector-ref words wi))
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
    [(struct bit-vector (words size word-size))
     (define word (vector-ref words wi))
     (define bit  (bitwise-bit-set? word bi))
     (unless (eq? bit b)
       (vector-set! words wi (bitwise-xor word (expt 2 bi))))]))

(define (bit-vector-length bv)
  (bit-vector-size bv))

(define bit-vector-copy*
  (let ([bit-vector-copy
         (case-lambda
          [(bv)
           (bit-vector (vector-copy (bit-vector-words bv))
                       (bit-vector-size bv)
                       (bit-vector-word-size bv))]
          [(bv start)
           (bit-vector-copy bv start)]
          [(bv start end)
           (bit-vector-copy bv start end)])])
    bit-vector-copy))

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
(struct bit-vector (words size word-size)
  ; words     is the vector of words
  ; size      is the number of bits in bitvector
  ; word-size is the number of words in words
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
           [ny (bit-vector-size y)]
           [wsx (bit-vector-word-size x)]
           [wsy (bit-vector-word-size y)])
       (and (= nx ny) (= wsx wsy) 
            (for/and ([index (in-range (- (vector-length vx) 1))])
              (eqv? (vector-ref vx index)
                    (vector-ref vy index)))
            ; TODO: check last word
            )))
   (define (hash-code x hc)
     (let ([v (bit-vector-words x)]
           [n (bit-vector-size x)]
           [ws (bit-vector-word-size x)])
       (bitwise-xor
        (hc ws) (hc n)
        (for/fold ([h 1]) ([i (in-range (vector-length v))])
          (bitwise-xor h (hc (vector-ref v i)))))))
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
 (rename bit-vector-copy*
         bit-vector-copy
         (-> bit-vector? bit-vector?)))

(provide in-bit-vector for/bit-vector for*/bit-vector)
