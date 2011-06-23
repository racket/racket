#;
(
TR opt: structs.rkt 13:0 (pt-x a) -- struct ref
TR opt: structs.rkt 14:0 (set-pt-y! a 5) -- struct set
3
)

#lang typed/scheme
#:optimize

(define-struct: pt ((x : Integer) (y : Integer)) #:mutable)
(define a (pt 3 4))
(pt-x a)
(set-pt-y! a 5)
