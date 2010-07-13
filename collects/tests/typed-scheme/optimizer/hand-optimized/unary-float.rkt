(module float-unary typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-flsin 2.0))
