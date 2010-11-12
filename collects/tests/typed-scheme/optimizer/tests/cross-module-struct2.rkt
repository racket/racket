#;
(
cross-module-struct2.rkt line 11 col 1 - x-x - struct ref
1
)

#lang typed/scheme #:optimize

(require (file "cross-module-struct.rkt"))
(define a (make-x 1))
(x-x a)
