#;#;
#<<END
TR opt: pair-fun.rkt 6:6 (car x) -- pair
END
""
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(: f ((Listof Integer) -> Integer))
(define (f x)
  (if (null? x)
      1
      (car x)))
