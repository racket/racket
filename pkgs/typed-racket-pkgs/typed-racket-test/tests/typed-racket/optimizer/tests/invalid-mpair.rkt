#;#;
#<<END
TR missed opt: invalid-mpair.rkt 12:2 (mcar x) -- car/cdr on a potentially empty list -- caused by: 12:8 x

END
""

#lang typed/scheme
#:optimize
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
