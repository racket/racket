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
(: non-235 (Listof Positive-Byte))
(define non-235 '(1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59))
; Note that there are exactly 16 of these moduli, so they fit in a u16.
; That is, a single u16 can represent a block of 60 numbers.

(: mod60->bits (Vectorof (U #f Byte)))
(define mod60->bits (make-vector 60 #f))
(for ([x : Positive-Byte (in-list non-235)]
      [b (in-naturals)])
  (vector-set! mod60->bits x (assert b byte?)))

(: mod60->bit (Byte -> (U #f Byte)))
(define (mod60->bit m) (vector-ref mod60->bits m))

(: *number-of-groups* Positive-Fixnum)
(define *number-of-groups* 17000) ; each group contain 16 numbers
(: *SMALL-PRIME-LIMIT* Nonnegative-Fixnum)
(define *SMALL-PRIME-LIMIT* (assert (- (* 60 *number-of-groups*) 1) fixnum?))

; primes holds (* 16 *number-of-groups*) bits
; each representing a number not congruent to 2, 3, 5
(define primes (make-bit-vector (* (length non-235) *number-of-groups*) #t))

(define: (clear-bit! [x : Nonnegative-Fixnum]) : Void
  (define q (quotient x 60))
  (define b (mod60->bit (remainder x 60)))
  (when b (bit-vector-set! primes (+ (* q 16) b) #f)))

(define: (inner-bit [q : Nonnegative-Fixnum] [r : Byte]) : Boolean
  (define b (mod60->bit r))
  (if b 
      (bit-vector-ref primes (+ (* q 16) b))
      #f))

(define: (bit [x : Nonnegative-Fixnum]) : Boolean
  (define q (quotient x 60))
  (define r (remainder x 60))
  (inner-bit q r))


(define: (mark-composites [x : Nonnegative-Fixnum]) : Void
  ; x is prime => mark n*x as prime for all n
  ; Well 2*x, 3*x, 4*x, 5*x, 6*x are not in our table,
  ; so only mark the multiples that are not divisible by 2, 3, or 5.
  (let/ec: exit : Void
    (let: loop : Void ([a : Nonnegative-Fixnum 0])
      (define y-base (* a 60 x))
      (for: ([d : Positive-Byte (in-list non-235)])
        (define y (assert (+ y-base (* d x)) fixnum?))
        (when (not (= y x))
          (if (<= y *SMALL-PRIME-LIMIT*)
              (clear-bit! y)
              (exit (void)))))
      (loop (assert (add1 a) fixnum?)))))

(define: sieve-done? : Boolean  #f)

(define: (sieve) : Void
  (clear-bit! 1) ; 1 is not prime
  (let/ec: exit : Void
    (let: loop : Void ([q : Nonnegative-Fixnum 0])
      (for: ([r : Positive-Byte (in-list non-235)])
        (define x (assert (+ (* q 60) r) fixnum?))
        (when (> (* x x) *SMALL-PRIME-LIMIT*)
          (exit (void)))
        (when (inner-bit q r) ; x is prime
          (mark-composites x)))
      (loop (assert (add1 q) fixnum?)))))

(define: (small-prime? [x : Nonnegative-Fixnum]) : Boolean
  (unless sieve-done?
    (sieve)
    (set! sieve-done? #t))
  (or (= x 2) (= x 3) (= x 5) (bit x)))
