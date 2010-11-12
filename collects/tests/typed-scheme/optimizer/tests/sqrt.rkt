#;
(
sqrt.rkt line 11 col 3 - sqrt - unary float
)

#lang typed/scheme
#:optimize

(: f (Nonnegative-Float -> Nonnegative-Float))
(define (f x)
  (sqrt x))
