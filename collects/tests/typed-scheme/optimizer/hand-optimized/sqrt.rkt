(module sqrt typed/scheme #:optimize
  (require racket/unsafe/ops)
  (: f (Nonnegative-Float -> Nonnegative-Float))
  (define (f x)
    (unsafe-flsqrt x)))
