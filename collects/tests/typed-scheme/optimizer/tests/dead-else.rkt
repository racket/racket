#;
(
#f line #f col #f - op - dead else branch
dead-else.rkt line 14 col 14 - + - binary float
#f line #f col #f - op - dead else branch
dead-else.rkt line 17 col 14 - + - binary float
5.05.0
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(display (if (number? 3)
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #t
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
