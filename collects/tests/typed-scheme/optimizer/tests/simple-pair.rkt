#;
(
TR opt: simple-pair.rkt 10:0 (car (cons 1 2)) -- pair
1
)

#lang typed/scheme
#:optimize

(car (cons 1 2))
