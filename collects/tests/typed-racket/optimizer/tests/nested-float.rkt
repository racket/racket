#;
(
TR opt: nested-float.rkt 11:0 (+ 2.0 (+ 3.0 4.0)) -- binary float
TR opt: nested-float.rkt 11:7 (+ 3.0 4.0) -- binary float
9.0
)

#lang typed/scheme
#:optimize

(+ 2.0 (+ 3.0 4.0))
