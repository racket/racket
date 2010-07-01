(module invalid-vector-set typed/scheme
  (vector-set! (vector 1 2) 0 2)) ; type is (Vectorof Integer), length is ot known, can't optimize
