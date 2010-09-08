#;
(
define-begin-float.rkt line 11 col 27 - - - binary float
define-begin-float.rkt line 12 col 18 - * - binary float
-1.0
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(define a (begin (display (- 2.0 3.0))
                 (* 2.0 3.0)))
