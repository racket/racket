#;
(
pair-fun.rkt 13:7 car -- pair
)

#lang typed/scheme
#:optimize

(: f ((Listof Integer) -> Integer))
(define (f x)
  (if (null? x)
      1
      (car x)))
