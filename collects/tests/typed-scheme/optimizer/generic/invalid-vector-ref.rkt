#lang typed/scheme
#:optimize
(: f ((Vectorof Integer) -> Integer))
(define (f x)
  (vector-ref x 0)) ; type is (Vectorof Integer), length is unknown, can't optimize
