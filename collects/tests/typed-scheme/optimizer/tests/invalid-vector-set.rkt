#;
(
TR opt: invalid-vector-set.rkt 10:2 (vector-set! x 0 2) -- vector access splitting
)

#lang typed/scheme
#:optimize
(: f ((Vectorof Integer) -> Void))
(define (f x)
  (vector-set! x 0 2)) ; type is (Vectorof Integer), length is ot known, can't optimize
