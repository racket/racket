#;
(
apply-plus.rkt line 12 col 7 - + - apply-map
apply-plus.rkt line 13 col 7 - * - apply-map
9
24
)

#lang typed/racket
#:optimize

(apply + (map add1 (list 1 2 3)))
(apply * (map add1 (list 1 2 3)))
