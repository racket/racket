#;
(
let-rhs.rkt line 13 col 10 - + - binary float
let-rhs.rkt line 13 col 0 - (let-values (((x) (#%app + (quote 1.0) (quote 2.0)))) x) - unboxed let bindings
3.0
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(let ((x (+ 1.0 2.0)))
  x)
