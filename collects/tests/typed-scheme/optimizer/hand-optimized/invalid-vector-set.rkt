(module invalid-vector-set typed/scheme
  (: f ((Vectorof Integer) -> Void))
  (define f
    (#%plain-lambda (x) (vector-set! x 0 2)))) ; type is (Vectorof Integer), length is ot known, can't optimize
