#lang typed/racket

(struct: foo ([x : Integer]) #:mutable)

(: f (Integer -> foo))
(define (f x) (foo x))
