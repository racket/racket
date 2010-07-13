(module double-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fl+ (unsafe-fl+ 2.0 2.0) 2.0))
