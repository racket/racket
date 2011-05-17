#;
(
dead-else.rkt line 15 col 13 - (#%app + (quote 4.0) (quote 5.0)) - dead else branch
dead-else.rkt line 14 col 14 - + - binary float
dead-else.rkt line 18 col 13 - (#%app + (quote 4.0) (quote 5.0)) - dead else branch
dead-else.rkt line 17 col 14 - + - binary float
5.05.0
)

#lang typed/scheme
#:optimize

(display (if (number? 3)
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #t
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
