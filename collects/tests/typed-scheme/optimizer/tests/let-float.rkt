#;
(
let-float.rkt line 11 col 10 - + - binary float
let-float.rkt line 12 col 3 - * - binary float
45.0
)

#lang typed/scheme
#:optimize

(let ((x (+ 3.0 2.0)))
  (* 9.0 x))
