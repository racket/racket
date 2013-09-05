#;#;
#<<END
TR opt: float-promotion.rkt 16:0 (+ (assert (modulo 1 2) exact-positive-integer?) 2.0) -- binary float
TR opt: float-promotion.rkt 16:11 (modulo 1 2) -- binary nonzero fixnum
TR opt: float-promotion.rkt 17:0 (+ (expt 100 100) 2.0) -- binary float
END
#<<END
3.0
1e+200

END

#lang typed/scheme
#:optimize

(+ (assert (modulo 1 2) exact-positive-integer?) 2.0)
(+ (expt 100 100) 2.0)
