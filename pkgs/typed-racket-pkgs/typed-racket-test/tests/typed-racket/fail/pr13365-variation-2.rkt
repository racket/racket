#;
(exn-pred #rx"expected: String\n  given:")
#lang typed/racket

(: f (All (a ...) (a ... a -> (Values String a ... a))))
(define (f . x) x)
