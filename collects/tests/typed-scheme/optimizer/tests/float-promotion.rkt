#;
(
TR opt: float-promotion.rkt 13:1 + -- binary float
TR opt: float-promotion.rkt 13:4 modulo -- binary nonzero fixnum
TR opt: float-promotion.rkt 14:1 + -- binary float
2.0
1e+200
)

#lang typed/scheme
#:optimize

(+ (modulo 1 1) 2.0)
(+ (expt 100 100) 2.0)
