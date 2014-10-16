#lang typed/racket

(define-type inf (Rec inf (Any -> inf)))
(: f inf)
(define (f x) f)

(: g (All (x) (x -> x)))
(define (g x) x)

(: v (Listof (U inf Number)))
(define v
  (list
    (with-handlers ((number? add1)) 3)
    (with-handlers ((void f)) 4)))


(list
  (with-handlers ((void values)) 6)
  (with-handlers ((number? add1)) 7)
  (with-handlers ((void f)) 8)
  (with-handlers ((void g)) 9))
