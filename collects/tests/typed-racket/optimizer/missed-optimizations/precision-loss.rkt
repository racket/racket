#;
(
TR missed opt: precision-loss.rkt 36:0 (+ (* 3/4 2/3) 2.0) -- exact ops inside float expr -- caused by: 36:3 (* 3/4 2/3)
TR opt: precision-loss.rkt 36:0 (+ (* 3/4 2/3) 2.0) -- binary float
TR opt: precision-loss.rkt 38:0 (+ 3/4 2.0) -- binary float
TR missed opt: precision-loss.rkt 40:0 (+ (- 3/4) 2.0) -- exact ops inside float expr -- caused by: 40:3 (- 3/4)
TR opt: precision-loss.rkt 40:0 (+ (- 3/4) 2.0) -- binary float
TR opt: precision-loss.rkt 42:0 (+ (vector-ref (quote #(2/3 1/2 3/4)) (assert (+ 1/4 3/4) exact-integer?)) 2.0) -- binary float
TR missed opt: precision-loss.rkt 48:8 (* 3/4 2/3) -- all args float-arg-expr, result not Float -- caused by: 48:11 3/4, 48:15 2/3
TR opt: precision-loss.rkt 49:3 (car (list (* 2.0 (ann (* 3/4 2/3) Real)))) -- pair
TR missed opt: precision-loss.rkt 49:26 (* 3/4 2/3) -- all args float-arg-expr, result not Float -- caused by: 49:29 3/4, 49:33 2/3
TR missed opt: precision-loss.rkt 49:14 (* 2.0 (ann (* 3/4 2/3) Real)) -- all args float-arg-expr, result not Float -- caused by: 49:26 (* 3/4 2/3)
TR missed opt: precision-loss.rkt 49:26 (* 3/4 2/3) -- all args float-arg-expr, result not Float -- caused by: 49:29 3/4, 49:33 2/3
TR missed opt: precision-loss.rkt 48:0 (* (ann (* 3/4 2/3) Real) (car (list (* 2.0 (ann (* 3/4 2/3) Real)))) 2.0) -- all args float-arg-expr, result not Float -- caused by: 48:8 (* 3/4 2/3), 49:3 (car (list (* 2.0 (ann (* 3/4 2/3) Real))))
TR missed opt: precision-loss.rkt 48:8 (* 3/4 2/3) -- all args float-arg-expr, result not Float -- caused by: 48:11 3/4, 48:15 2/3
TR opt: precision-loss.rkt 49:3 (car (list (* 2.0 (ann (* 3/4 2/3) Real)))) -- pair
TR missed opt: precision-loss.rkt 49:26 (* 3/4 2/3) -- all args float-arg-expr, result not Float -- caused by: 49:29 3/4, 49:33 2/3
TR missed opt: precision-loss.rkt 49:14 (* 2.0 (ann (* 3/4 2/3) Real)) -- all args float-arg-expr, result not Float -- caused by: 49:26 (* 3/4 2/3)
TR missed opt: precision-loss.rkt 49:26 (* 3/4 2/3) -- all args float-arg-expr, result not Float -- caused by: 49:29 3/4, 49:33 2/3
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
   (car (list (* 2.0 (ann (* 3/4 2/3) Real))))
   2.0)
