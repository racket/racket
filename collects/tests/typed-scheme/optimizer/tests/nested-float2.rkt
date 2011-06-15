#;
(
TR opt: nested-float2.rkt 11:1 + -- binary float
TR opt: nested-float2.rkt 11:8 * -- binary float
14.0
)

#lang typed/scheme
#:optimize

(+ 2.0 (* 3.0 4.0))
