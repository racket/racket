#;
(
precision-loss.rkt 24:0 (#%app + (#%app * (quote 3/4) (quote 2/3)) (quote 2.0)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 24:3 (#%app * (quote 3/4) (quote 2/3))
precision-loss.rkt 24:1 + -- binary float
precision-loss.rkt 26:1 + -- binary float
precision-loss.rkt 28:0 (#%app + (#%app - (quote 3/4)) (quote 2.0)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 28:3 (#%app - (quote 3/4))
precision-loss.rkt 28:1 + -- binary float
precision-loss.rkt 30:1 + -- binary float
precision-loss.rkt 36:0 (#%app * (#%app * (quote 3/4) (quote 2/3)) (quote 2.0)) -- binary, args all float-arg-expr, return type not Float -- caused by: 36:11 (quote 3/4), 36:15 (quote 2/3)
precision-loss.rkt 36:0 (#%app * (#%app * (quote 3/4) (quote 2/3)) (quote 2.0)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 36:8 (#%app * (quote 3/4) (quote 2/3))
2.5
2.75
1.25
2.5
1.0
 )


#lang typed/racket

;; warn when the extra precision gained by doing exact computations would
;; be lost when the results are mixed with floats, resulting in extra
;; computation cost for (usually) no gain
(+ (* 3/4 2/3) ; exact computation
   2.0) ; extra precision lost
(+ 3/4 2.0) ; here, since the exact subexpression is atomic, it will get
;; coerced anyway, so there's not much need for a warning
(+ (- 3/4) ; should work on unary exprs too
   2.0)
(+ (vector-ref '#(2/3 1/2 3/4) (assert (+ 1/4 3/4) exact-integer?)) ; and this is not an arith expr
   2.0)

;; in this case, the return type is Real, so we can't optimize
;; however, given that the return _value_ will be a float, the precision
;; is thrown away nonetheless, so a warning is warranted
(* (ann (* 3/4 2/3) Real)
   2.0)
