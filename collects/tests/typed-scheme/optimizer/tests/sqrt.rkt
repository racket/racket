#;
(
TR opt: sqrt.rkt 11:2 (sqrt x) -- unary float
)

#lang typed/scheme
#:optimize

(: f (Nonnegative-Float -> Nonnegative-Float))
(define (f x)
  (sqrt x))
