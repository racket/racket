(module simple-pair typed/scheme #:optimize
  (require racket/unsafe/ops)
  (car (cons 1 2)))
