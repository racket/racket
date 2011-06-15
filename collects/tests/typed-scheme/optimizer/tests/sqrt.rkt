#;
(
TR opt: sqrt.rkt 11:3 sqrt -- unary float
)

#lang typed/scheme
#:optimize

(: f (Nonnegative-Float -> Nonnegative-Float))
(define (f x)
  (sqrt x))
