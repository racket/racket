#;
(
nested-float2.rkt line 11 col 8 - * - binary float
nested-float2.rkt line 11 col 1 - + - binary float
14.0
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(+ 2.0 (* 3.0 4.0))
