#lang typed/racket

(: f1 (case-> (Number -> Number) (Number 4 -> Number)))
(define (f1 x (y 4)) y)

(f1 2 4)

(: f2 (4 -> (Values Number Number)))
(define (f2 x) (values x x))

(f2 4)

(: f3 (4 Number * -> Number))
(define (f3 x . y) x)

(f3 4)

(: f4 (4 * -> Number))
(define (f4 . y) 5)

(f4 4 4)

