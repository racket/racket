(module let-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (let ((x (unsafe-fl+ 3.0 2.0)))
    (unsafe-fl* 9.0 x)))
