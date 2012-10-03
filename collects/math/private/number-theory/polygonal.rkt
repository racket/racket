#lang typed/racket/base

(require "quadratic.rkt"
         (only-in "number-theory.rkt" perfect-square)
         "types.rkt")

(provide triangle triangle?
         square?
         pentagonal pentagonal?
         hexagonal hexagonal?
         heptagonal heptagonal?
         octagonal octagonal?)

(: triangle : Natural -> Natural)
(define (triangle n)
  (quotient (* n (+ n 1)) 2))

(: triangle? : Natural -> Boolean)
(define (triangle? n)
  (not (null? (quadratic-natural-solutions 1/2 1/2 (- n)))))

(: square? : Natural -> Boolean)
(define (square? n)
  (and (perfect-square n) #t))

(: pentagonal : Natural -> Natural)
(define (pentagonal n)
  (assert (quotient (* n (- (* 3 n) 1)) 2) natural?))

(: pentagonal? : Natural -> Boolean)
(define (pentagonal? n)
  (not (null? (quadratic-natural-solutions 3/2 -1/2 (- n)))))

(: hexagonal : Natural -> Natural)
(define (hexagonal n)
  (assert (* n (- (* 2 n) 1)) natural?))

(: hexagonal? : Natural -> Boolean)
(define (hexagonal? n)
  (not (null? (quadratic-natural-solutions 2 -1 (- n)))))

(: heptagonal : Natural -> Natural)
(define (heptagonal n)
  (assert (quotient (* n (- (* 5 n) 3)) 2) natural?))

(: heptagonal? : Natural -> Boolean)
(define (heptagonal? n)
  (not (null? (quadratic-natural-solutions 5/2 -3/2 (- n)))))

(: octagonal : Natural -> Natural)
(define (octagonal n)
  (assert (* n (- (* 3 n) 2)) natural?))

(: octagonal? : Natural -> Boolean)
(define (octagonal? n)
  (not (null? (quadratic-natural-solutions 3 -2 (- n)))))
