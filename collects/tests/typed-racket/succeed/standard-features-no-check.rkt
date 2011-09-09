#lang typed/racket/no-check

(: f (Integer -> Any))
(define (f x) (add1 ""))

(lambda (#{x : String}) (string-append " " x))

