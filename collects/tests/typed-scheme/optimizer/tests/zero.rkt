#;
(
TR opt: zero.rkt 13:0 (zero? 1) -- fixnum zero?
TR opt: zero.rkt 14:0 (zero? (sqrt 3.0)) -- float zero?
TR opt: zero.rkt 14:7 (sqrt 3.0) -- unary float
#f
#f
)

#lang typed/scheme
#:optimize

(zero? 1)
(zero? (sqrt 3.0))
