#;
(
TR opt: float-real.rkt 18:0 (+ 2.3 (ann 3 Positive-Real)) -- binary float
TR missed opt: float-real.rkt 19:15 (* (ann 2 Integer) 3.2) -- all args float-arg-expr, result not Float -- caused by: 19:23 2
TR info: float-real.rkt 19:15 (* (ann 2 Integer) 3.2) -- exact real arith
TR opt: float-real.rkt 19:0 (+ 2.3 (assert (* (ann 2 Integer) 3.2) positive?)) -- binary float
TR missed opt: float-real.rkt 20:7 (* (ann 2 Integer) 3.1) -- all args float-arg-expr, result not Float -- caused by: 20:15 2
TR info: float-real.rkt 20:7 (* (ann 2 Integer) 3.1) -- exact real arith
TR missed opt: float-real.rkt 20:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- all args float-arg-expr, result not Float -- caused by: 20:7 (* (ann 2 Integer) 3.1)
TR info: float-real.rkt 20:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- exact real arith
5.3
8.7
14.26
)

#lang typed/racket
;; reals within float expressions should be coerced when it's safe to do so
(+ 2.3 (ann 3 Positive-Real)) ; safe
(+ 2.3 (assert (* (ann 2 Integer) 3.2) positive?)) ; inner = unsafe, outer = unsafe
(* 2.3 (* (ann 2 Integer) 3.1)) ; all unsafe
