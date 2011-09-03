#;
(
TR opt: pair-fun.rkt 13:6 (car x) -- pair
)

#lang typed/scheme
#:optimize

(: f ((Listof Integer) -> Integer))
(define (f x)
  (if (null? x)
      1
      (car x)))
