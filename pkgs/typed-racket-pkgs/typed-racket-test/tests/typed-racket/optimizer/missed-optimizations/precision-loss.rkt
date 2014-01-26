#;#;
#<<END
TR info: precision-loss.rkt 40:3 (* 3/4 2/3) -- possible exact real arith
TR info: precision-loss.rkt 44:3 (- 3/4) -- possible exact real arith
TR info: precision-loss.rkt 46:39 (+ 1/4 3/4) -- possible exact real arith
TR info: precision-loss.rkt 52:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0) -- possible exact real arith
TR info: precision-loss.rkt 52:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0) -- possible exact real arith
TR info: precision-loss.rkt 52:3 (* (r 3/4) 2/3) -- possible exact real arith
TR info: precision-loss.rkt 53:14 (* 2.0 (* (r 3/4) 2/3)) -- possible exact real arith
TR info: precision-loss.rkt 53:21 (* (r 3/4) 2/3) -- possible exact real arith
TR missed opt: precision-loss.rkt 40:0 (+ (* 3/4 2/3) 2.0) -- exact ops inside float expr -- caused by: 40:3 (* 3/4 2/3)
TR missed opt: precision-loss.rkt 44:0 (+ (- 3/4) 2.0) -- exact ops inside float expr -- caused by: 44:3 (- 3/4)
TR missed opt: precision-loss.rkt 46:0 (+ (vector-ref (quote #(2/3 1/2 3/4)) (assert (+ 1/4 3/4) exact-integer?)) 2.0) -- all args float-arg-expr, result not Float -- caused by: 46:3 (vector-ref (quote #(2/3 1/2 3/4)) (assert (+ 1/4 3/4) exact-integer?))
TR missed opt: precision-loss.rkt 52:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0) -- all args float-arg-expr, result not Float -- caused by: 52:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0)
TR missed opt: precision-loss.rkt 52:0 (* (* (r 3/4) 2/3) (car (list (* 2.0 (* (r 3/4) 2/3)))) 2.0) -- all args float-arg-expr, result not Float -- caused by: 52:3 (* (r 3/4) 2/3), 53:3 (car (list (* 2.0 (* (r 3/4) 2/3))))
TR missed opt: precision-loss.rkt 52:3 (* (r 3/4) 2/3) -- all args float-arg-expr, result not Float -- caused by: 52:6 (r 3/4), 52:14 2/3
TR missed opt: precision-loss.rkt 53:14 (* 2.0 (* (r 3/4) 2/3)) -- all args float-arg-expr, result not Float -- caused by: 53:21 (* (r 3/4) 2/3)
TR missed opt: precision-loss.rkt 53:21 (* (r 3/4) 2/3) -- all args float-arg-expr, result not Float -- caused by: 53:24 (r 3/4), 53:32 2/3
TR opt: precision-loss.rkt 40:0 (+ (* 3/4 2/3) 2.0) -- binary float
TR opt: precision-loss.rkt 42:0 (+ 3/4 2.0) -- binary float
TR opt: precision-loss.rkt 44:0 (+ (- 3/4) 2.0) -- binary float
TR opt: precision-loss.rkt 53:3 (car (list (* 2.0 (* (r 3/4) 2/3)))) -- pair
END
#<<END
2.5
2.75
1.25
2.5
1.0

END


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
