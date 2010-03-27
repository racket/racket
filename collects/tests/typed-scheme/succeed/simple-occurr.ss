#lang typed/scheme

(: x (U #f Number))
(define x 7)

(if x (add1 x) 7)

(if (number? x) (add1 x) 7)