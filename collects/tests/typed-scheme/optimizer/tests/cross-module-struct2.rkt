#;
(
TR opt: cross-module-struct2.rkt 11:0 (x-x a) -- struct ref
1
)

#lang typed/scheme #:optimize

(require (file "cross-module-struct.rkt"))
(define a (make-x 1))
(x-x a)
