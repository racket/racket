(module define-begin-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define a (begin (display (- 2.0 3.0))
                   (* 2.0 3.0))))
