(module simple-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fl+ 2.0 3.0))
