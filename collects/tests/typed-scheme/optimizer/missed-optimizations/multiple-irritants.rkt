#;
(
 multiple-irritants.rkt 9:0 (#%app * (quote 4) (quote 5) (quote 6.0)) -- binary, args all float-arg-expr, return type not Float -- caused by: 9:8 (quote 4), 9:24 (quote 5)
120.0
)

#lang typed/racket

(* (ann 4 Integer) (ann 5 Integer) 6.0)
