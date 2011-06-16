#;
(
TR opt: float-real.rkt 18:1 + -- binary float
TR missed opt: float-real.rkt 19:0 (+ 2.3 (* (ann 2 Integer) 3.2)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 19:7 (* (ann 2 Integer) 3.2)
TR opt: float-real.rkt 19:1 + -- binary float
TR missed opt: float-real.rkt 19:7 (* (ann 2 Integer) 3.2) -- binary, args all float-arg-expr, return type not Float -- caused by: 19:15 2
TR missed opt: float-real.rkt 20:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- binary, args all float-arg-expr, return type not Float -- caused by: 20:15 2 (2 times)
TR missed opt: float-real.rkt 20:0 (* 2.3 (* (ann 2 Integer) 3.1)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 20:7 (* (ann 2 Integer) 3.1)
5.3
8.7
14.26
)


#lang typed/racket

;; reals within float expressions should be coerced when it's safe to do so
(+ 2.3 (ann 3 Real)) ; safe
(+ 2.3 (* (ann 2 Integer) 3.2)) ; inner = unsafe, outer = safe
(* 2.3 (* (ann 2 Integer) 3.1)) ; all unsafe
