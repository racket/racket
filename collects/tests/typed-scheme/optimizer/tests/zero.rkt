#;
(
zero.rkt 13:1 zero? -- fixnum zero?
zero.rkt 14:1 zero? -- float zero?
zero.rkt 14:8 sqrt -- unary float
#f
#f
)

#lang typed/scheme
#:optimize

(zero? 1)
(zero? (sqrt 3.0))
