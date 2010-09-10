#;
(
nested-pair1.rkt line 11 col 6 - cdr - pair
nested-pair1.rkt line 11 col 1 - car - pair
2
)

#lang typed/scheme
#:optimize

(car (cdr '(1 2)))
