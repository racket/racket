#;
(
TR missed opt: all-real.rkt 24:0 (+ (ann 3 Real) (ann 4 Real)) -- all args float-arg-expr, result not Float -- caused by: 24:8 3, 24:21 4
TR missed opt: all-real.rkt 25:0 (* (ann 3 Real) (ann 4 Real)) -- all args float-arg-expr, result not Float -- caused by: 25:8 3, 25:21 4
7
12
)

#lang typed/racket

;; all-real expressions
;; They currently get reported as missed float optimizations.
;; Whether that's the right thing or not is debatable.
;; pro: more often than not, Real types pop up when the type system can't
;;      prove something, but the user really wants a float type, so reporting
;;      is likely to be helpful
;; con: all-real expressions can arise from integer-only computations
;;      (ex: (+ (sqrt 4) 2) : Positive-Real), so reporting a missed float opt
;;      sounds wrong
;; If we decide against reporting these cases in the future, it's only a matter
;; of adding a check to make sure at least one of the subexpressions is in the
;; Float layer in addition to the entire expression being in the Real layer.

(+ (ann 3 Real) (ann 4 Real))
(* (ann 3 Real) (ann 4 Real))
