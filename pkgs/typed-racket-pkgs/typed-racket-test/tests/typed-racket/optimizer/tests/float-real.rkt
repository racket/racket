#;#;
#<<END
TR info: float-real.rkt 22:15 (* (ann 2 Integer) 3.2) -- exact real arith
TR info: float-real.rkt 23:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- exact real arith
TR info: float-real.rkt 23:7 (* (ann 2 Integer) 3.1) -- exact real arith
TR missed opt: float-real.rkt 22:15 (* (ann 2 Integer) 3.2) -- all args float-arg-expr, result not Float -- caused by: 22:23 2
TR missed opt: float-real.rkt 23:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- all args float-arg-expr, result not Float -- caused by: 23:7 (* (ann 2 Integer) 3.1)
TR missed opt: float-real.rkt 23:7 (* (ann 2 Integer) 3.1) -- all args float-arg-expr, result not Float -- caused by: 23:15 2
TR opt: float-real.rkt 21:0 (+ 2.3 (ann 3 Positive-Real)) -- binary float
TR opt: float-real.rkt 22:0 (+ 2.3 (assert (* (ann 2 Integer) 3.2) positive?)) -- binary float
END
#<<END
5.3
8.7
14.26

END

#lang typed/racket
;; reals within float expressions should be coerced when it's safe to do so
(+ 2.3 (ann 3 Positive-Real)) ; safe
(+ 2.3 (assert (* (ann 2 Integer) 3.2) positive?)) ; inner = unsafe, outer = unsafe
(* 2.3 (* (ann 2 Integer) 3.1)) ; all unsafe
