#;
(exn-pred #rx"Expected String, but got")
#lang typed/racket

(: f (All (a ...) (a ... a -> (Values String a ... a))))
(define (f . x) x)
