#;#;
#<<END
TR opt: mpair.rkt 13:6 (mcar x) -- pair
TR opt: mpair.rkt 4:0 (mcar x) -- pair
TR opt: mpair.rkt 5:0 (mcdr x) -- pair
TR opt: mpair.rkt 6:0 (set-mcar! x (+ 1 2)) -- pair
TR opt: mpair.rkt 6:13 (+ 1 2) -- fixnum bounded expr
TR opt: mpair.rkt 7:0 (set-mcdr! x (+ 1.0 2.0)) -- pair
TR opt: mpair.rkt 7:13 (+ 1.0 2.0) -- binary float
END
#<<END
1
1.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(: x (MPairof Integer Float))
(define x (mcons 1 1.0))
(mcar x)
(mcdr x)
(set-mcar! x (+ 1 2))
(set-mcdr! x (+ 1.0 2.0))

(: f ((MListof Integer) -> Integer))
(define (f x)
  (if (null? x)
      0
      (mcar x)))
