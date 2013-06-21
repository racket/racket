#;#;
#<<END
TR opt: mpair.rkt 23:0 (mcar x) -- pair
TR opt: mpair.rkt 24:0 (mcdr x) -- pair
TR opt: mpair.rkt 25:0 (set-mcar! x (+ 1 2)) -- pair
TR opt: mpair.rkt 25:13 (+ 1 2) -- fixnum bounded expr
TR opt: mpair.rkt 26:0 (set-mcdr! x (+ 1.0 2.0)) -- pair
TR opt: mpair.rkt 26:13 (+ 1.0 2.0) -- binary float
TR opt: mpair.rkt 32:6 (mcar x) -- pair

END
#<<END
1
1.0

END

#lang typed/scheme
#:optimize

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
