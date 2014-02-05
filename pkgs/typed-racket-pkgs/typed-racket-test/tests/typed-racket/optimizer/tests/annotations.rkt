#;#;
#<<END
TR opt: annotations.rkt 10:15 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 2:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 5:10 (+ 1.0 2.0) -- binary float
TR opt: annotations.rkt 8:10 (+ 1.0 2.0) -- binary float
END
""
#lang typed/racket #:optimize
#reader tests/typed-racket/optimizer/reset-port

(define a (+ 1.0 2.0))

(: b Float)
(define b (+ 1.0 2.0))

(: c Any)
(define c (+ 1.0 2.0))

(define d (ann (+ 1.0 2.0) Any))
