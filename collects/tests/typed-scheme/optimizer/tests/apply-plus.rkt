#;
(
TR opt: apply-plus.rkt 12:7 + -- apply-map
TR opt: apply-plus.rkt 13:7 * -- apply-map
9
24
)

#lang typed/racket
#:optimize

(apply + (map add1 (list 1 2 3)))
(apply * (map add1 (list 1 2 3)))
