#;
(
TR opt: float-real.rkt 18:0 (+ 2.3 (ann 3 Real)) -- binary float
TR missed opt: float-real.rkt 19:0 (+ 2.3 (* (ann 2 Integer) 3.2)) -- exact ops inside float expr -- caused by: 19:7 (* (ann 2 Integer) 3.2)
TR opt: float-real.rkt 19:0 (+ 2.3 (* (ann 2 Integer) 3.2)) -- binary float
TR missed opt: float-real.rkt 19:7 (* (ann 2 Integer) 3.2) -- all args float-arg-expr, result not Float -- caused by: 19:15 2
TR missed opt: float-real.rkt 20:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- exact ops inside float expr -- caused by: 20:7 (* (ann 2 Integer) 3.1)
TR missed opt: float-real.rkt 20:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- all args float-arg-expr, result not Float -- caused by: 20:15 2 (2 times)
5.3
8.7
14.26
)


#lang typed/racket

;; reals within float expressions should be coerced when it's safe to do so
(+ 2.3 (ann 3 Real)) ; safe
(+ 2.3 (* (ann 2 Integer) 3.2)) ; inner = unsafe, outer = safe
(* 2.3 (* (ann 2 Integer) 3.1)) ; all unsafe
