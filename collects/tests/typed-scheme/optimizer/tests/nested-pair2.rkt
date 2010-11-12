#;
(
nested-pair2.rkt line 11 col 6 - cdr - pair
nested-pair2.rkt line 11 col 1 - car - pair
'(2)
)

#lang typed/scheme
#:optimize

(car (cdr (cons 3 (cons (cons 2 '()) 1))))
