#;
(
TR opt: let-float.rkt 11:10 + -- binary float
TR opt: let-float.rkt 12:3 * -- binary float
45.0
)

#lang typed/scheme
#:optimize

(let ((x (+ 3.0 2.0)))
  (* 9.0 x))
