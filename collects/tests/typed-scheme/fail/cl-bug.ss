#lang typed-scheme

(: f3 (case-lambda (Integer * -> Integer) (Number * -> Number)))
(define (f3 x y) (+ x y))

(: f2 (case-lambda (Number * -> Number)))
(define (f2 x y) (+ x y))
