#;#;
#<<END
TR info: float-real.rkt 3:15 (* (ann 2 Integer) 3.2) -- possible exact real arith
TR info: float-real.rkt 4:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- possible exact real arith
TR info: float-real.rkt 4:7 (* (ann 2 Integer) 3.1) -- possible exact real arith
TR missed opt: float-real.rkt 3:15 (* (ann 2 Integer) 3.2) -- all args float-arg-expr, result not Float -- caused by: 3:23 2
TR missed opt: float-real.rkt 4:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- all args float-arg-expr, result not Float -- caused by: 4:7 (* (ann 2 Integer) 3.1)
TR missed opt: float-real.rkt 4:7 (* (ann 2 Integer) 3.1) -- all args float-arg-expr, result not Float -- caused by: 4:15 2
TR opt: float-real.rkt 2:0 (+ 2.3 (ann 3 Positive-Real)) -- binary float
TR opt: float-real.rkt 3:0 (+ 2.3 (assert (* (ann 2 Integer) 3.2) positive?)) -- binary float
END
#<<END
5.3
8.7
14.26

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
;; reals within float expressions should be coerced when it's safe to do so
(+ 2.3 (ann 3 Positive-Real)) ; safe
(+ 2.3 (assert (* (ann 2 Integer) 3.2) positive?)) ; inner = unsafe, outer = unsafe
(* 2.3 (* (ann 2 Integer) 3.1)) ; all unsafe
