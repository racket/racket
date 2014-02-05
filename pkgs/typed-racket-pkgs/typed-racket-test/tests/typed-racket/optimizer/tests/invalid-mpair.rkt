#;#;
#<<END
TR missed opt: invalid-mpair.rkt 3:2 (mcar x) -- car/cdr on a potentially empty list -- caused by: 3:8 x
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(: f ((MListof Integer) -> Integer))
(define (f x)
  (mcar x))
