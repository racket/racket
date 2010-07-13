(module nested-pair typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-car (unsafe-cdr '(1 2))))
