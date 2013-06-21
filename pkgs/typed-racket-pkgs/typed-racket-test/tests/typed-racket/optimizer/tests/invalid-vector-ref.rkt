#;#;
#<<END
TR opt: invalid-vector-ref.rkt 11:2 (vector-ref x 0) -- vector partial bounds checking elimination
END
""

#lang typed/scheme
#:optimize
(: f ((Vectorof Integer) -> Integer))
(define (f x)
  (vector-ref x 0)) ; type is (Vectorof Integer), length is unknown, can't optimize
