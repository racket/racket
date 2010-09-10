#;
(
begin-float.rkt line 12 col 8 - - - binary float
begin-float.rkt line 13 col 8 - * - binary float
-1.0
6.0
)

#lang typed/scheme
#:optimize

(begin (- 2.0 3.0)
       (* 2.0 3.0))
