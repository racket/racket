#;
(
TR opt: nested-pair2.rkt 11:1 car -- pair
TR opt: nested-pair2.rkt 11:6 cdr -- pair
'(2)
)

#lang typed/scheme
#:optimize

(car (cdr (cons 3 (cons (cons 2 '()) 1))))
