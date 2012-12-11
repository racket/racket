#lang typed/racket
(require/typed 
 data/bit-vector               
 [#:opaque BitVector bit-vector?]
 [make-bit-vector (Integer Boolean -> BitVector)]
 [bit-vector-set! (BitVector Integer Boolean -> Void)]
 [bit-vector-ref  (BitVector Integer -> Boolean)])

(provide small-prime?
         *SMALL-PRIME-LIMIT*)

; The moduli mod 60 that 2, 3 and 5 do not divide are:
(define non-235 '(1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59))
; The differences of these numbers are:
(define deltas  '( 6 4  2  4  2  4  6  2  6  4  2  4  2  4  6  2))
; Note that there are exactly 16 of these moduli, so they fit in a u16.
; That is, a single u16 can represent a block of 60 numbers.

(define mod60->bits (make-vector 60 (cast #f (U #f Integer))))
(for ([x (in-list non-235)]
      [b (in-naturals)])
  (vector-set! mod60->bits x b))

(define-syntax-rule (mod60->bit m) (vector-ref mod60->bits m))

(define *number-of-groups* 17000) ; each group contain 16 numbers
(define *SMALL-PRIME-LIMIT* (- (* 60 *number-of-groups*) 1))

; primes holds (* 60 *number-of-groups*) bits each
; representing a number not congruent to 2, 3, 5
(define primes (make-bit-vector (* 60 *number-of-groups*) #t))

(define: (set-bit! [x : Integer]) : Void
  (define-values (q r) (quotient/remainder x 60))
  (define b (mod60->bit r))
  (when b (bit-vector-set! primes (+ (* q 16) b) #t)))

(define: (clear-bit! [x : Integer]) : Void
  (define-values (q r) (quotient/remainder x 60))
  (define b (mod60->bit r))
  (when b (bit-vector-set! primes (+ (* q 16) b) #f)))

(define: (bit [x : Integer]) : Boolean
  (define-values (q r) (quotient/remainder x 60))
  (define b (mod60->bit r))
  (if b 
      (bit-vector-ref primes (+ (* q 16) b))
      #f))

(clear-bit! 1) ; 1 is not prime

(define: (mark-composites [x : Integer]) : Void
  ; x is prime => mark 2*x, 3*x, 4*x, 5*x, 6*x, 7*x, ... as prime
  ; Well 2*x, 3*x, 4*x, 5*x, 6*x are not in our table,
  ; so the first number to mark is 7*x .
  ; Use the deltas to figure out which to mark.  
  (define y x)
  (define delta*x 0)
  (let loop ([ds deltas])
    ; (for ([delta (in-cycle deltas)] ...
    (when (empty? ds) (set! ds deltas))
    (let ([delta (car ds)])
      (set! delta*x (* delta x))
      (cond
        [(> y (- *SMALL-PRIME-LIMIT* delta*x))
         (void)]
        [else         
         (set! y (+ y delta*x))
         (clear-bit! y)
         (loop (cdr ds))]))))

(define: (sieve) : Void
  (define x 1)
  (let loop ([ds deltas])
    ; (for ([delta (in-cycle deltas)] ...
    (when (empty? ds) (set! ds deltas))
    (let ([delta (car ds)])
      (cond
        [(> (* x x) (- *SMALL-PRIME-LIMIT* delta))
         (void)]
        [else
         ; x runs through all numbers incongruent to 2, 3 and 5
         (set! x (+ x delta))
         (when (bit x) ; x is prime
           (mark-composites x))
         (loop (cdr ds))]))))

(sieve)

(define: (small-prime? [x : Integer]) : Boolean
  (or (= x 2) (= x 3) (= x 5)
      (and (mod60->bit (modulo x 60))
           (bit x))))
