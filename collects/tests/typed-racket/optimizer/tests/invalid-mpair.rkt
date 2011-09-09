#;
(
TR missed opt: invalid-mpair.rkt 10:2 (mcar x) -- car/cdr on a potentially empty list -- caused by: 10:8 x
)

#lang typed/scheme
#:optimize
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
