#;
(
TR opt: float-promotion.rkt 13:0 (+ (modulo 1 1) 2.0) -- binary float
TR opt: float-promotion.rkt 13:3 (modulo 1 1) -- binary nonzero fixnum
TR opt: float-promotion.rkt 14:0 (+ (expt 100 100) 2.0) -- binary float
2.0
1e+200
)

#lang typed/scheme
#:optimize

(+ (modulo 1 1) 2.0)
(+ (expt 100 100) 2.0)
