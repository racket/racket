#lang racket/base
(require 
 ; Uncomment the representation you want to benchmark
 ; data/bit-vector-bignum
 data/bit-vector
 racket/math)

(define *small-prime-limit* 100000)
(define *largest-index* (quotient *small-prime-limit* 2))
(define small-odd-primes (make-bit-vector (quotient *small-prime-limit* 2) #t))
; (bit-vector-red small-odd-primes i) <=> 2*i+1 is prime

(define (sieve)
  (bit-vector-set! small-odd-primes 0 #f) ; 1 is composite
  (bit-vector-set! small-odd-primes 1 #t) ; 3 is prime
  (for ([i (in-range 1 (quotient (exact-ceiling (sqrt *small-prime-limit*)) 1))])
    (when (bit-vector-ref small-odd-primes i)
      (for ([j (in-range (+ (* 3 i) 1) *largest-index* (+ (* 2 i) 1))])
        (bit-vector-set! small-odd-primes j #f)))))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(time (sieve))
