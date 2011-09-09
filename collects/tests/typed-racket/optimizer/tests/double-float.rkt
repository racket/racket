#;
(
TR opt: double-float.rkt 10:0 (+ 2.0 2.0 2.0) -- binary float
6.0
)

#lang typed/scheme
#:optimize

(+ 2.0 2.0 2.0)
