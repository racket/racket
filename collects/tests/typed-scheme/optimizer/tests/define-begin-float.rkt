#;
(
TR opt: define-begin-float.rkt 10:26 (- 2.0 3.0) -- binary float
TR opt: define-begin-float.rkt 11:17 (* 2.0 3.0) -- binary float
-1.0)

#lang typed/scheme
#:optimize

(define a (begin (display (- 2.0 3.0))
                 (* 2.0 3.0)))
