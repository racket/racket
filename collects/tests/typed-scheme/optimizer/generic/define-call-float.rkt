(module define-call-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define x (cons (+ 1.0 2.0) 3.0)))
