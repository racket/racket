#;
(
begin-float.rkt 12:8 - -- binary float
begin-float.rkt 13:8 * -- binary float
-1.0
6.0
)

#lang typed/scheme
#:optimize

(begin (- 2.0 3.0)
       (* 2.0 3.0))
