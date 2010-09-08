#;
(
define-call-float.rkt line 9 col 17 - + - binary float
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(define x (cons (+ 1.0 2.0) 3.0))
