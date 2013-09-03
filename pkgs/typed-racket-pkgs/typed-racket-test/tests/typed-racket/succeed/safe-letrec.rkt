#lang typed/racket

;; make sure letrec takes into account that some bindings may be undefined

(letrec: ([x : Number 3])
  x)
(letrec: ([x : Number 3]
          [y : (-> Number) (lambda () x)]) ; lambdas are safe
  y)
(letrec: ([a : (-> Void) (lambda () (b))]
          [b : (-> Void) (lambda () (a))])
  a)
(letrec: ([x : (Number -> Number) (lambda (y) (+ y 3))]
          [y : Number (x 4)])
  y)
(letrec-values: ([([a : (-> Number)]) (lambda () 3)]
                 [([b : (-> Number)]) (lambda () (a))]
                 [([x : Number] [y : Number])  (values (b) (b))])
  x)
(letrec: ([x : Number 3]
          [y : (Number -> Number) (lambda (x) (if z 0 1))] ; not transitively safe, but safe
          [z : Number x])
  z)
