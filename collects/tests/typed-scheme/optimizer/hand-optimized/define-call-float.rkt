(module define-call-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define x (cons (unsafe-fl+ 1.0 2.0) 3.0)))
