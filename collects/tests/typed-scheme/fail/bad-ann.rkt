#;
(exn-pred 2)
#lang typed/scheme


(: f : Number -> Number)
(define (f a b)
  (+ a b))

(define: (g [a : Number] [b : Number]) : Number
  (+ a b))

(f 1 2)
(g 1 2)
