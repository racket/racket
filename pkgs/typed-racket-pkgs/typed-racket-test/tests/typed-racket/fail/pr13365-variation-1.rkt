#;
(exn-pred #rx"expected: 0 values and `a ...'")
#lang typed/racket

(: f (All (a ...) (a ... a -> (Values a ... a))))
(define (f . x) x)
