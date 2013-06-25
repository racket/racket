#lang typed/racket

(define-type inf (Rec inf (Any -> inf)))
(: f inf)
(define (f x) f)

(: g (All (x) (x -> x)))
(define (g x) x)

(: v (Listof (U inf Byte)))
(define v
  (list
    (with-handlers ((void values)) 2)
    (with-handlers ((void add1)) 3)
    (with-handlers ((void f)) 4)
    (with-handlers ((void g)) 5)))


(list
  (with-handlers ((void values)) 6)
  (with-handlers ((void add1)) 7)
  (with-handlers ((void f)) 8)
  (with-handlers ((void g)) 9))
