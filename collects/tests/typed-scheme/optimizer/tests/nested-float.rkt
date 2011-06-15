#;
(
TR opt: nested-float.rkt 11:1 + -- binary float
TR opt: nested-float.rkt 11:8 + -- binary float
9.0
)

#lang typed/scheme
#:optimize

(+ 2.0 (+ 3.0 4.0))
