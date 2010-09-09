#;
(
simple-pair.rkt line 10 col 1 - car - pair
1
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(car (cons 1 2))
