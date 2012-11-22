#lang racket


(define ((bad-index-error who index))
  (raise-mismatch-error who "index out of range" index))

(define (bit-vector-ref bv n
          [default (bad-index-error 'gvector-ref n)])
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'bit-vector-ref "exact nonnegative integer" n))
  
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
             

(define (bit-vector-count bv)
  (bit-vector-size bv))

(define bits-in-a-word
  (if (fixnum? (expt 2 61)) 
      ; 32 or 64-bit fixnums?
      62 30))

(define (make-bit-vector size)
  ; make a bit-vector with size bits
  (define word-size (add1 (quotient size bits-in-a-word)))
  (define words (make-vector word-size 0))
  (bit-vector words size word-size))



(define (bit-vector-set! bv n b)
  (unless (boolean? b)
    (error 'bit-vector-set! "expected boolean as third argument, got ~a" b))
  
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  (match bv
    [(struct bit-vector (words size word-size))
     (define word (vector-ref words wi))
     (define bit  (bitwise-bit-set? word bi))
     (unless (eq? bit b)
       (vector-set! words wi (bitwise-xor word (expt 2 bi))))]))

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
                          bit-vector-count
                          bit-vector-iterate-first
                          bit-vector-iterate-next
                          bit-vector-iterate-key
                          bit-vector-iterate-value)
        (vector-immutable exact-nonnegative-integer?
                          boolean?
                          exact-nonnegative-integer?
                          #f #f #f))
  ; #:methods gen:equal+hash
  ; [(define (equal-proc x y recursive-equal?) ...)
  ;  (define (hash-code x hc) ...)
  ;  (define hash-proc  hash-code)
  ;  (define hash2-proc hash-code)]
  ;#:property prop:sequence in-bit-vector
  )

(define bv (make-bit-vector 500))
(bit-vector-set! bv 4 #t)
(bit-vector-ref bv 4)
(bit-vector-set! bv 4 #f)
(bit-vector-ref bv 4)
(bit-vector-set! bv 4 #t)
(bit-vector-ref bv 4)
(bit-vector-set! bv 4 #f)
(bit-vector-ref bv 4)

(bit-vector-set! bv 400 #t)
(bit-vector-ref bv 400)
(bit-vector-set! bv 400 #f)
(bit-vector-ref bv 400)
(bit-vector-set! bv 400 #t)
(bit-vector-ref bv 400)
(bit-vector-set! bv 400 #t)
(bit-vector-ref bv 400)
