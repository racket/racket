#;
(
TR opt: define-call-float.rkt 9:16 (+ 1.0 2.0) -- binary float
)

#lang typed/scheme
#:optimize

(define x (cons (+ 1.0 2.0) 3.0))
