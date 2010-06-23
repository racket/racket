(module define-pair typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define x (unsafe-car '(1 3))))
