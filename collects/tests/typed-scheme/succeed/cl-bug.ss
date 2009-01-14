#lang typed-scheme

(: f (case-lambda (Integer * -> Integer) (Number * -> Number)))
(define (f . x) (+ 1 2))

(: f4 (case-lambda (Integer * -> Integer) (Number * -> Number)))
(define (f4 . x) (apply + x))
