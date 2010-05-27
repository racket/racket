#lang typed/racket

(: f (All (a ...) ((List a ...) -> (List a ... a))))
(define (f x) x)

(: g (All (a ...) (a ... -> (List a ...))))
(define (g . x) x)

(g 7 7 7)
