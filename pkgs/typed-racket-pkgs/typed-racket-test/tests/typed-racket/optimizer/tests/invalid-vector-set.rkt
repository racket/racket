#;#;
#<<END
TR opt: invalid-vector-set.rkt 3:2 (vector-set! x 0 2) -- vector partial bounds checking elimination
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(: f ((Vectorof Integer) -> Void))
(define (f x)
  (vector-set! x 0 2)) ; type is (Vectorof Integer), length is ot known, can't optimize
