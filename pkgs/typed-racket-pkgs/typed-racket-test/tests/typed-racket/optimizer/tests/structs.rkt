#;#;
#<<END
TR info: structs.rkt 2:11 pt -- struct constructor
TR opt: structs.rkt 3:0 (pt-x a) -- struct ref
TR opt: structs.rkt 4:0 (set-pt-y! a 5) -- struct set
END
#<<END
3

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(define-struct: pt ((x : Integer) (y : Integer)) #:mutable)
(define a (pt 3 4))
(pt-x a)
(set-pt-y! a 5)
