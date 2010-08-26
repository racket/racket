#lang typed/scheme
#:optimize
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
