#lang typed/racket

(: f (All (a ...) ((List a ...) -> (List a ... a))))
(define (f x) x)

(ann (values (inst f String Number Boolean)) String)
