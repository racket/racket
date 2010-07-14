#lang typed/scheme
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
