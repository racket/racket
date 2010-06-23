(module float-promotion typed/scheme #:optimize
  (require racket/unsafe/ops)
  (/ 1 2.0))
