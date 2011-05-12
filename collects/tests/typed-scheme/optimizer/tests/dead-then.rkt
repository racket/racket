#;
(
dead-then.rkt 14:13 (#%app + (quote 2.0) (quote 3.0)) -- dead then branch
dead-then.rkt 15:14 + -- binary float
dead-then.rkt 17:13 (#%app + (quote 2.0) (quote 3.0)) -- dead then branch
dead-then.rkt 18:14 + -- binary float
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
