(module nested-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fl+ 2.0 (unsafe-fl+ 3.0 4.0)))
