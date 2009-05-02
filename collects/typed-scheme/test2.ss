#lang typed-scheme

(: f (Number String -> Number))
(define (f x z) (f x z))
(lambda: ([x : Any] [y : Any]) (values (number? y) (number? x)))
(lambda: ([x : Any] [y : Any]) (values (number? x) (number? y)))
(lambda: ([x : Any]) (values (number? x) (number? x)))
(: g (Any -> Boolean : Number))
(define g (lambda: ([x : Any]) (number? x)))

;(f 12 "hi")

