#lang typed-scheme

(: f (Number String -> Number))
(define (f x z) #;(f x z) 7)
(lambda: ([x : Any] [y : Any]) (values (number? y) (number? x)))
(lambda: ([x : Any] [y : Any]) (values (number? x) (number? y)))
(lambda: ([x : Any] [y : Any]) (values (and (number? x) (boolean? y)) (number? y)))
(lambda: ([x : Any]) (values (number? x) (number? x)))
(: g (Any -> Boolean : Number))
(define g (lambda: ([x : Any]) (number? x)))
(: q ((Number -> Number) -> Number))
(define q (lambda: ([x : (Number -> Number)]) (x 1)))
;(q (lambda (z) (f z "foo")))

(: p (Number * -> Number))
(define (p . x) 7)

(lambda x (number? x))
(+)
(+ 1 2 3)
(+ 1 2 3.5)

;(f 12 "hi")

