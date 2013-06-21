#;#;
#<<END
TR opt: annotations.rkt 11:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 14:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 17:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 19:15 (+ 1.0 2.0) -- binary float
END
""
#lang typed/racket #:optimize

(define a (+ 1.0 2.0))

(: b Float)
(define b (+ 1.0 2.0))

(: c Any)
(define c (+ 1.0 2.0))

(define d (ann (+ 1.0 2.0) Any))
