(module vector-set typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-vector*-set! (ann (vector 1 2) (Vector Integer Integer))
                       0
                       1))
