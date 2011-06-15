#;
(
TR opt: mpair.rkt 19:1 mcar -- mutable pair
TR opt: mpair.rkt 20:1 mcdr -- mutable pair
TR opt: mpair.rkt 21:1 set-mcar! -- mutable pair
TR opt: mpair.rkt 21:14 + -- fixnum bounded expr
TR opt: mpair.rkt 22:1 set-mcdr! -- mutable pair
TR opt: mpair.rkt 22:14 + -- binary float
TR opt: mpair.rkt 28:7 mcar -- mutable pair
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
