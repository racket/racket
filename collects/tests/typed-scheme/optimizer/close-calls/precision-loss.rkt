#;
(
precision-loss.rkt 13:3 (#%app * (quote 3/4) (quote 2/3)) -- exact arithmetic subexpression inside a float expression, extra precision discarded -- caused by: 13:0 (#%app + (#%app * (quote 3/4) (quote 2/3)) (quote 2.0))              
2.5
2.75
 )

#lang typed/racket

;; warn when the extra precision gained by doing exact computations would
;; be lost when the results are mixed with floats, resulting in extra
;; computation cost for (usually) no gain
(+ (* 3/4 2/3) ; exact computation
   2.0) ; extra precision lost
(+ 3/4 2.0) ; here, since the exact subexpression is atomic, it will get
;; coerced anyway, so there's not much need for a warning
