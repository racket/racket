(module invalid-vector-ref typed/scheme
  (: f ((Vectorof Integer) -> Integer))
  (define f
    (#%plain-lambda (x) (vector-ref x 0)))) ; type is (Vectorof Integer), length is unknown, can't optimize
