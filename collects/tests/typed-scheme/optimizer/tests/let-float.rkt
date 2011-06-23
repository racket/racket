#;
(
TR opt: let-float.rkt 11:9 (+ 3.0 2.0) -- binary float
TR opt: let-float.rkt 12:2 (* 9.0 x) -- binary float
45.0
)

#lang typed/scheme
#:optimize

(let ((x (+ 3.0 2.0)))
  (* 9.0 x))
