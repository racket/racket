#;
(
TR info: precision-loss.rkt 36:3 (* 3/4 2/3) -- exact real arith
TR missed opt: precision-loss.rkt 36:0 (+ (* 3/4 2/3) 2.0) -- exact ops inside float expr -- caused by: 36:3 (* 3/4 2/3)
TR opt: precision-loss.rkt 36:0 (+ (* 3/4 2/3) 2.0) -- binary float
TR opt: precision-loss.rkt 38:0 (+ 3/4 2.0) -- binary float
TR info: precision-loss.rkt 40:3 (- 3/4) -- exact real arith
TR missed opt: precision-loss.rkt 40:0 (+ (- 3/4) 2.0) -- exact ops inside float expr -- caused by: 40:3 (- 3/4)
TR opt: precision-loss.rkt 40:0 (+ (- 3/4) 2.0) -- binary float
TR info: precision-loss.rkt 42:39 (+ 1/4 3/4) -- exact real arith
TR missed opt: precision-loss.rkt 42:0 (+ (vector-ref (quote #(2/3 1/2 3/4)) (assert (+ 1/4 3/4) exact-integer?)) 2.0) -- all args float-arg-expr, result not Float -- caused by: 42:3 (vector-ref (quote #(2/3 1/2 3/4)) (assert (+ 1/4 3/4) exact-integer?))
TR info: precision-loss.rkt 42:39 (+ 1/4 3/4) -- exact real arith
TR missed opt: precision-loss.rkt 48:3 (* (r 3/4) 2/3) -- all args float-arg-expr, result not Float -- caused by: 48:6 (r 3/4), 48:14 2/3
TR info: precision-loss.rkt 48:3 (* (r 3/4) 2/3) -- exact real arith
TR opt: precision-loss.rkt 49:3 (car (list (* 2.0 (* (r 3/4) 2/3)))) -- pair
TR missed opt: precision-loss.rkt 49:21 (* (r 3/4) 2/3) -- all args float-arg-expr, result not Float -- caused by: 49:24 (r 3/4), 49:32 2/3
TR info: precision-loss.rkt 49:21 (* (r 3/4) 2/3) -- exact real arith
TR missed opt: precision-loss.rkt 49:14 (* 2.0 (* (r 3/4) 2/3)) -- all args float-arg-expr, result not Float -- caused by: 49:21 (* (r 3/4) 2/3)
TR info: precision-loss.rkt 49:14 (* 2.0 (* (r 3/4) 2/3)) -- exact real arith
TR missed opt: precision-loss.rkt 48:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0) -- all args float-arg-expr, result not Float -- caused by: 48:3 (* (r 3/4) 2/3), 49:3 (car (list (* 2.0 (* (r 3/4) 2/3))))
TR info: precision-loss.rkt 48:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0) -- exact real arith
2.5
2.75
1.25
2.5
1.0
)


#lang typed/racket
(: r (Real -> Real))
(define (r x) x)
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
(* (* (r 3/4) 2/3)
   (car (list (* 2.0 (* (r 3/4) 2/3))))
   2.0)
