#;
(
structs.rkt line 13 col 1 - pt-x - struct ref
structs.rkt line 14 col 1 - set-pt-y! - struct set
3
)

#lang typed/scheme
#:optimize

(define-struct: pt ((x : Integer) (y : Integer)) #:mutable)
(define a (pt 3 4))
(pt-x a)
(set-pt-y! a 5)
