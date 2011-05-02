#;
(
 real-in-float-expr.rkt line 10 col 0 - (#%app * (quote 3) (quote 2.3)) - binary, args all float-arg-expr, return type not Float
 6.8999999999999995
 6
 5
 )
#lang typed/racket

(* (ann 3 Real) ; with type Real, typechecker must assume it could be exact 0
   2.3)

(* (ann 2 Integer) (ann 3 Integer)) ; but these have nothing to do with floats, should not be logged
(+ (ann 2 Integer) (ann 3 Integer))
