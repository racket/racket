#;
(
define-pair.rkt line 9 col 11 - car - pair
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(define x (car '(1 3)))
