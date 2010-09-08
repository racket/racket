#;
(
let-float.rkt line 12 col 10 - + - binary float
let-float.rkt line 12 col 0 - (let-values (((x) (#%app + (quote 3.0) (quote 2.0)))) (#%app * (quote 9.0) x)) - unboxed let bindings
let-float.rkt line 13 col 3 - * - binary float
45.0
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(let ((x (+ 3.0 2.0)))
  (* 9.0 x))
