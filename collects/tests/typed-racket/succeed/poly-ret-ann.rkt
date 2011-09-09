
#lang typed/scheme/base
(: f (Integer -> (All (X) (X -> X))))
(define (f x)
 (add1 x)
 (lambda (x) x))
