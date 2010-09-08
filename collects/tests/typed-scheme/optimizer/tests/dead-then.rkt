#;
(
#f line #f col #f - op - dead then branch
dead-then.rkt line 15 col 14 - + - binary float
#f line #f col #f - op - dead then branch
dead-then.rkt line 18 col 14 - + - binary float
9.09.0
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(display (if (number? "eh")
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #f
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
