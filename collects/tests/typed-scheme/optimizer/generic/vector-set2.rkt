(module invalid-vector-set typed/scheme #:optimize
  (require racket/unsafe/ops)
  (vector-set! (vector 1 2) 0 2)) ; type is (Vectorof Integer), length is ot known, can't optimize
