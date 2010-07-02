(module float-comp typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fl< 1.0 2.0))
