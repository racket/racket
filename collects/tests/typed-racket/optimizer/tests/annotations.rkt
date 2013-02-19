#;
(
TR opt: annotations.rkt 10:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 13:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 16:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 18:15 (+ 1.0 2.0) -- binary float
)
#lang typed/racket #:optimize

(define a (+ 1.0 2.0))

(: b Float)
(define b (+ 1.0 2.0))

(: c Any)
(define c (+ 1.0 2.0))

(define d (ann (+ 1.0 2.0) Any))
