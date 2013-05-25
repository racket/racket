#;
(
TR opt: define-pair.rkt 9:10 (car (quote (1 3))) -- pair
)

#lang typed/scheme
#:optimize

(define x (car '(1 3)))
