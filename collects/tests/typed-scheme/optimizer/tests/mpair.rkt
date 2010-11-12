#;
(
mpair.rkt line 18 col 1 - mcar - mutable pair
mpair.rkt line 19 col 1 - mcdr - mutable pair
mpair.rkt line 20 col 1 - set-mcar! - mutable pair
mpair.rkt line 21 col 1 - set-mcdr! - mutable pair
mpair.rkt line 21 col 14 - + - binary float
mpair.rkt line 27 col 7 - mcar - mutable pair
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
