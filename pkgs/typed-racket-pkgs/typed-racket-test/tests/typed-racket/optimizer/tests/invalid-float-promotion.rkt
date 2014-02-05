#;#;
#<<END
TR info: invalid-float-promotion.rkt 2:0 (/ (ann 1 Integer) 2.0) -- possible exact real arith
TR info: invalid-float-promotion.rkt 3:0 (* (ann 2/3 Exact-Rational) 3.0) -- possible exact real arith
TR missed opt: invalid-float-promotion.rkt 2:0 (/ (ann 1 Integer) 2.0) -- all args float-arg-expr, result not Float -- caused by: 2:8 1
TR missed opt: invalid-float-promotion.rkt 3:0 (* (ann 2/3 Exact-Rational) 3.0) -- all args float-arg-expr, result not Float -- caused by: 3:8 2/3
END
#<<END
0.5
2.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
;; the ann are necessary, since (* PosReal Float) -> Float (exact 0 is not in PosReal)
(/ (ann 1 Integer) 2.0) ; result is not a float, can't optimize
(* (ann 2/3 Exact-Rational) 3.0)
