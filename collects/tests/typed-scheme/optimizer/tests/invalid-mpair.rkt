#;
(
TR missed opt: invalid-mpair.rkt 10:2 (mcar x) -- mpair op on a potentially empty mlist -- caused by: 10:8 x
)

#lang typed/scheme
#:optimize
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
