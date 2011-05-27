#;
(
dead-else.rkt 14:14 + -- binary float
dead-else.rkt 15:13 (#%app + (quote 4.0) (quote 5.0)) -- dead else branch
dead-else.rkt 17:14 + -- binary float
dead-else.rkt 18:13 (#%app + (quote 4.0) (quote 5.0)) -- dead else branch
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
