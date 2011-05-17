#;
(
dead-then.rkt line 14 col 13 - (#%app + (quote 2.0) (quote 3.0)) - dead then branch
dead-then.rkt line 15 col 14 - + - binary float
dead-then.rkt line 17 col 13 - (#%app + (quote 2.0) (quote 3.0)) - dead then branch
dead-then.rkt line 18 col 14 - + - binary float
9.09.0
)

#lang typed/scheme
#:optimize

(display (if (number? "eh")
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #f
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
