#;
(
TR opt: define-begin-float.rkt 11:27 - -- binary float
TR opt: define-begin-float.rkt 12:18 * -- binary float
-1.0
)

#lang typed/scheme
#:optimize

(define a (begin (display (- 2.0 3.0))
                 (* 2.0 3.0)))
