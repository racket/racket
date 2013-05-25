#lang typed/scheme/base

(: f (Integer -> Any))
(define (f x) (add1 x))

(lambda (#{x : String}) (string-append " " x))

