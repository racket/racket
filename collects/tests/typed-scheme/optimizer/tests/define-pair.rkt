#;
(
define-pair.rkt line 9 col 11 - car - unary pair
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(define x (car '(1 3)))
