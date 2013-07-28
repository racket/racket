#lang typed/scheme

(define x (vector 1.0 2.0)) ; should generalize to (Vectorof Float) even though it only contains Nonnegative-Floats
(vector-set! x 0 -2.0)

(define y (make-vector 2 1.0))
(vector-set! y 0 -2.0)

(define z #(1.0 2.0))
(ann z (Vectorof Float))

(define w (build-vector 3 (lambda: ((x : Integer)) (add1 x))))
(vector-set! w 0 -2)
