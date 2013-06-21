#;#;
#<<END
TR info: structs.rkt 15:11 pt -- struct constructor
TR opt: structs.rkt 16:0 (pt-x a) -- struct ref
TR opt: structs.rkt 17:0 (set-pt-y! a 5) -- struct set
END
#<<END
3

END

#lang typed/scheme
#:optimize
(define-struct: pt ((x : Integer) (y : Integer)) #:mutable)
(define a (pt 3 4))
(pt-x a)
(set-pt-y! a 5)
