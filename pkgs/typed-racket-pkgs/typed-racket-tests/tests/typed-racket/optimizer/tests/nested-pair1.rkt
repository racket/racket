#;
(
TR opt: nested-pair1.rkt 11:0 (car (cdr (quote (1 2)))) -- pair
TR opt: nested-pair1.rkt 11:5 (cdr (quote (1 2))) -- pair
2
)

#lang typed/scheme
#:optimize

(car (cdr '(1 2)))
