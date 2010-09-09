#;
(
pair-fun.rkt line 13 col 7 - car - pair
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(: f ((Listof Integer) -> Integer))
(define (f x)
  (if (null? x)
      1
      (car x)))
