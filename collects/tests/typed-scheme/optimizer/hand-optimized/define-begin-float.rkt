(module define-begin-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define a (begin (display (unsafe-fl- 2.0 3.0))
                   (unsafe-fl* 2.0 3.0))))
