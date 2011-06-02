#;
(
real-in-float-expr.rkt 17:0 (#%app * (quote 3) (quote 2.3)) -- binary, args all float-arg-expr, return type not Float -- caused by: 17:8 (quote 3)
real-in-float-expr.rkt 22:1 * -- fixnum bounded expr
real-in-float-expr.rkt 23:1 + -- fixnum bounded expr
real-in-float-expr.rkt 26:0 (#%app * (quote 2) (quote 2.0)) -- binary, args all float-arg-expr, return type not Float -- caused by: 26:8 (quote 2)
 6.8999999999999995
 6
 5
 6
 5
 17/12
 4.0
 )
#lang typed/racket

(* (ann 3 Real) ; with type Real, typechecker must assume it could be exact 0
   2.3)

(* (ann 2 Integer) (ann 3 Integer)) ; but these have nothing to do with floats, should not be logged
(+ (ann 2 Integer) (ann 3 Integer))
(* 2 3)
(+ 2 3)
(+ 2/3 3/4)

(* (ann 2 Natural) 2.0) ; close calls that result in Nonnegative-Real and co (i.e. not directly Real) should be reported too
