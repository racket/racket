#;
(
0.5
2.0
)

#lang typed/scheme
#:optimize
;; the ann are necessary, since (* PosReal Float) -> Float (exact 0 is not in PosReal)
(/ (ann 1 Integer) 2.0) ; result is not a float, can't optimize
(* (ann 2/3 Exact-Rational) 3.0)
