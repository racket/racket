#lang typed-scheme

(: f (case-lambda (Integer * -> Integer) (Number * -> Number)))
(define (f . x) (+ 1 2))

(: f4 (case-lambda (Integer * -> Integer) (Number * -> Number)))
(define (f4 . x) (apply + x))

(: f3 (case-lambda (Integer * -> Integer) (Number * -> Number)))
(define (f3 x y) (+ x y))

(+ 1 'foo)

(: f2 (case-lambda (Number * -> Number)))
(define (f2 x y) (+ x y))