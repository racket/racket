(module begin-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (begin (unsafe-fl- 2.0 3.0)
         (unsafe-fl* 2.0 3.0)))
