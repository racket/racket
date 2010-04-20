#lang typed/scheme

(: x (U #f Number))
(define x 7)

(if x (add1 x) 7)

(if (number? x) (add1 x) 7)

(define: y : (Pair Any Any) (cons 1 2))

(if (number? (car y)) (add1 (car y)) 7)
