#lang typed/scheme


(: add (case-lambda (Integer -> Integer) (Integer Integer -> Integer)))
(define add
  (case-lambda
    [(x) (add x 0)]
    [(x y) (+ x y)]))
