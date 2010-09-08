#;
(
sqrt.rkt line 11 col 3 - sqrt - unary float
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(: f (Nonnegative-Float -> Nonnegative-Float))
(define (f x)
  (sqrt x))
