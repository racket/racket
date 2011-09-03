#;
(
TR missed opt: multiple-irritants.rkt 9:0 (* (ann 4 Integer) (ann 5 Integer) 6.0) -- all args float-arg-expr, result not Float -- caused by: 9:8 4, 9:24 5
120.0
)

#lang typed/racket

(* (ann 4 Integer) (ann 5 Integer) 6.0)
