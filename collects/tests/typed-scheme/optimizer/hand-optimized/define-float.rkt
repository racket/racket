(module define-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define x (unsafe-fl+ 1.0 2.0)))
