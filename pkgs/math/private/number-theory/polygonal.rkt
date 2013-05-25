#lang typed/racket/base

(require "quadratic.rkt"
         (only-in "number-theory.rkt" perfect-square)
         "types.rkt")

(provide triangle-number triangle-number?
         square-number?
         pentagonal-number pentagonal-number?
         hexagonal-number hexagonal-number?
         heptagonal-number heptagonal-number?
         octagonal-number octagonal-number?)

(: triangle-number : Natural -> Natural)
(define (triangle-number n)
  (quotient (* n (+ n 1)) 2))

(: triangle-number? : Natural -> Boolean)
(define (triangle-number? n)
  (not (null? (quadratic-natural-solutions 1/2 1/2 (- n)))))

(: square-number? : Natural -> Boolean)
(define (square-number? n)
  (and (perfect-square n) #t))

(: pentagonal-number : Natural -> Natural)
(define (pentagonal-number n)
  (assert (quotient (* n (- (* 3 n) 1)) 2) natural?))

(: pentagonal-number? : Natural -> Boolean)
(define (pentagonal-number? n)
  (not (null? (quadratic-natural-solutions 3/2 -1/2 (- n)))))

(: hexagonal-number : Natural -> Natural)
(define (hexagonal-number n)
  (assert (* n (- (* 2 n) 1)) natural?))

(: hexagonal-number? : Natural -> Boolean)
(define (hexagonal-number? n)
  (not (null? (quadratic-natural-solutions 2 -1 (- n)))))

(: heptagonal-number : Natural -> Natural)
(define (heptagonal-number n)
  (assert (quotient (* n (- (* 5 n) 3)) 2) natural?))

(: heptagonal-number? : Natural -> Boolean)
(define (heptagonal-number? n)
  (not (null? (quadratic-natural-solutions 5/2 -3/2 (- n)))))

(: octagonal-number : Natural -> Natural)
(define (octagonal-number n)
  (assert (* n (- (* 3 n) 2)) natural?))

(: octagonal-number? : Natural -> Boolean)
(define (octagonal-number? n)
  (not (null? (quadratic-natural-solutions 3 -2 (- n)))))
