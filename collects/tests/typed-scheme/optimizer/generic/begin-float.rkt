(module begin-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (begin (- 2.0 3.0)
         (* 2.0 3.0)))
