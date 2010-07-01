(module invalid-vector-ref typed/scheme
  (vector-ref (vector 1 2 3) 0)) ; type is (Vectorof Integer), length is unknown, can't optimize
