#;#;
#<<END
TR missed opt: invalid-mpair.rkt 11:2 (mcar x) -- car/cdr on a potentially empty list -- caused by: 11:8 x
END
""

#lang typed/scheme
#:optimize
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
