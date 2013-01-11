#;
(exn-pred #rx"Expected a ...")
#lang typed/racket

(: f (All (a ...) (a ... a -> (Values a ... a))))
(define (f . x) (values 1))
