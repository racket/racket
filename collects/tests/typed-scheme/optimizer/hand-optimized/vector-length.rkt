(module vector-length typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-vector*-length (vector 1 2 3)))
