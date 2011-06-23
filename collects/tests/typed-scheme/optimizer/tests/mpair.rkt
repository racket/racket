#;
(
TR opt: mpair.rkt 19:0 (mcar x) -- mutable pair
TR opt: mpair.rkt 20:0 (mcdr x) -- mutable pair
TR opt: mpair.rkt 21:0 (set-mcar! x (+ 1 2)) -- mutable pair
TR opt: mpair.rkt 21:13 (+ 1 2) -- fixnum bounded expr
TR opt: mpair.rkt 22:0 (set-mcdr! x (+ 1.0 2.0)) -- mutable pair
TR opt: mpair.rkt 22:13 (+ 1.0 2.0) -- binary float
TR opt: mpair.rkt 28:6 (mcar x) -- mutable pair
1
1.0
)

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
