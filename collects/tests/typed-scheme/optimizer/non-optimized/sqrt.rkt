(module sqrt typed/scheme 
  (require racket/unsafe/ops)
  (: f (Nonnegative-Float -> Nonnegative-Float))
  (define (f x)
    (sqrt x)))
