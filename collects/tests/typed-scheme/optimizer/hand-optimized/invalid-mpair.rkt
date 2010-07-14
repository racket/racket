#lang typed/scheme
(: f ((MListof Integer) -> Integer))
(define f
  (#%plain-lambda (x) (mcar x)))
