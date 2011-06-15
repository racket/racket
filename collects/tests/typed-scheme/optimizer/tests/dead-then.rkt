#;
(
TR opt: dead-then.rkt 13:13 (+ 2.0 3.0) -- dead then branch
TR opt: dead-then.rkt 14:14 + -- binary float
TR opt: dead-then.rkt 16:13 (+ 2.0 3.0) -- dead then branch
TR opt: dead-then.rkt 17:14 + -- binary float
9.09.0)

#lang typed/scheme
#:optimize

(display (if (number? "eh")
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #f
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
