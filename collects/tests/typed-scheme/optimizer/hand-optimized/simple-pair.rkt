(module simple-pair typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-car (cons 1 2)))
