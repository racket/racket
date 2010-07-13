(module vector-ref typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-vector*-ref (ann (vector 1 2) (Vector Integer Integer)) 0))
