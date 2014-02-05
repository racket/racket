#;#;
#<<END
TR info: real-in-float-expr.rkt 11:0 (* (ann 2 Natural) 2.0) -- possible exact real arith
TR info: real-in-float-expr.rkt 2:0 (* (ann 3 Real) 2.3) -- possible exact real arith
TR info: real-in-float-expr.rkt 9:0 (+ 2/3 3/4) -- possible exact real arith
TR missed opt: real-in-float-expr.rkt 11:0 (* (ann 2 Natural) 2.0) -- all args float-arg-expr, result not Float -- caused by: 11:8 2
TR missed opt: real-in-float-expr.rkt 2:0 (* (ann 3 Real) 2.3) -- all args float-arg-expr, result not Float -- caused by: 2:8 3
TR opt: real-in-float-expr.rkt 7:0 (* 2 3) -- fixnum bounded expr
TR opt: real-in-float-expr.rkt 8:0 (+ 2 3) -- fixnum bounded expr
END
#<<END
6.8999999999999995
6
5
6
5
17/12
4.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(* (ann 3 Real) ; with type Real, typechecker must assume it could be exact 0
   2.3)

(* (ann 2 Integer) (ann 3 Integer)) ; but these have nothing to do with floats, should not be logged
(+ (ann 2 Integer) (ann 3 Integer))
(* 2 3)
(+ 2 3)
(+ 2/3 3/4)

(* (ann 2 Natural) 2.0) ; close calls that result in Nonnegative-Real and co (i.e. not directly Real) should be reported too
