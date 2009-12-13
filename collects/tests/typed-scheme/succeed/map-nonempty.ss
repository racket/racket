#lang typed/scheme

(: x (Pair Number (Listof Number)))
(define x (cons 1 (list 1 2 3 4)))

(apply max (ann (map add1 x) : (Pair Number (Listof Number))))