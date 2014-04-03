#;
(exn-pred #rx"expected: 1 value and `a ...'\n  given: 1 value")
#lang typed/racket

(: f (All (a ...) (a ... a -> (Values String a ... a))))
(define (f . x) x)
