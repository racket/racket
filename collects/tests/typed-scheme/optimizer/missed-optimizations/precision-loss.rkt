#;
(
precision-loss.rkt 16:3 (#%app * (quote 3/4) (quote 2/3)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 16:0 (#%app + (#%app * (quote 3/4) (quote 2/3)) (quote 2.0))
precision-loss.rkt 20:3 (#%app - (quote 3/4)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 20:0 (#%app + (#%app - (quote 3/4)) (quote 2.0))
2.5
2.75
1.25
2.5
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
