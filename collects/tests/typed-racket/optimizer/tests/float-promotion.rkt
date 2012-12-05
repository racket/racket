#;
(
TR opt: float-promotion.rkt 14:11 (modulo 1 2) -- binary nonzero fixnum
TR opt: float-promotion.rkt 14:11 (modulo 1 2) -- binary nonzero fixnum
TR opt: float-promotion.rkt 14:0 (+ (assert (modulo 1 2) exact-positive-integer?) 2.0) -- binary float
TR opt: float-promotion.rkt 15:0 (+ (expt 100 100) 2.0) -- binary float
3.0
1e+200
)

#lang typed/scheme
#:optimize

(+ (assert (modulo 1 2) exact-positive-integer?) 2.0)
(+ (expt 100 100) 2.0)
