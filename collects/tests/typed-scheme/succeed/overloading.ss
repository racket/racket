#lang typed-scheme

(: f (case-lambda (Number -> Number) (Boolean -> Boolean)))
(define (f x) (if (number? x) 1 #f))

(: x Boolean)
(define x (f #t))

(: xx Number)
(define xx (f 0))
