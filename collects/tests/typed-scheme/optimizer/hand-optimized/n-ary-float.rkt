(module n-ary-float typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fl+ (unsafe-fl+ 1.0 2.0) 3.0))
