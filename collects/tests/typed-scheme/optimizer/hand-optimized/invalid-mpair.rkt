#lang typed/scheme
(: f ((MListof Integer) -> Integer))
(define f
  (lambda (x) (mcar x)))
