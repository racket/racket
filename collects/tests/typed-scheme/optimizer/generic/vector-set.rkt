(module vector-set typed/scheme #:optimize
  (require racket/unsafe/ops)
  (vector-set! (ann (vector 1 2) (Vector Integer Integer))
               0
               1))
