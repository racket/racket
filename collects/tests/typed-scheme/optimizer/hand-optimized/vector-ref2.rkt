(module vector-ref2 typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-vector*-ref (vector 1 2 3) 0))
