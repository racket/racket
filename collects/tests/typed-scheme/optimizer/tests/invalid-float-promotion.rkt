#;
(
TR missed opt: invalid-float-promotion.rkt 12:0 (/ (ann 1 Integer) 2.0) -- binary, args all float-arg-expr, return type not Float -- caused by: 12:8 (quote 1)
TR missed opt: invalid-float-promotion.rkt 13:0 (* (ann 2/3 Exact-Rational) 3.0) -- binary, args all float-arg-expr, return type not Float -- caused by: 13:8 (quote 2/3)
0.5
2.0
)

#lang typed/scheme
#:optimize
;; the ann are necessary, since (* PosReal Float) -> Float (exact 0 is not in PosReal)
(/ (ann 1 Integer) 2.0) ; result is not a float, can't optimize
(* (ann 2/3 Exact-Rational) 3.0)
