(module vector-length typed/scheme #:optimize
  (require racket/unsafe/ops)
  (vector-length (vector 1 2 3)))
