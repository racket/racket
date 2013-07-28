#lang typed/racket

(: f : Nonnegative-Integer -> Boolean)
(define (f x)
  (cond [(= 0 x) #t]
        [else (f (sub1 x))])) ; here, x is Positive-Integer
