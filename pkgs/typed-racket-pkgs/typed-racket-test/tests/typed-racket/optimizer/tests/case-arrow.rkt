#;#;
#<<END
TR missed opt: case-arrow.rkt 43:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 43:18 max, 43:22 min
TR info: case-arrow.rkt 43:15 (- max min) -- exact real arith
TR missed opt: case-arrow.rkt 43:12 (* (- max min) x) -- all args float-arg-expr, result not Float -- caused by: 43:15 (- max min), 43:27 x
TR info: case-arrow.rkt 43:12 (* (- max min) x) -- exact real arith
TR missed opt: case-arrow.rkt 43:9 (/ (* (- max min) x) p) -- all args float-arg-expr, result not Float -- caused by: 43:12 (* (- max min) x), 43:30 p
TR info: case-arrow.rkt 43:9 (/ (* (- max min) x) p) -- exact real arith
TR missed opt: case-arrow.rkt 43:2 (+ min (/ (* (- max min) x) p)) -- all args float-arg-expr, result not Float -- caused by: 43:5 min, 43:9 (/ (* (- max min) x) p)
TR info: case-arrow.rkt 43:2 (+ min (/ (* (- max min) x) p)) -- exact real arith

END
""
#lang typed/racket















;; Typechecking functions with case-> types causes the body to be typechecked
;; multiple times, which is fine, except that it used to cause the type table
;; to only have information for the last branch (clobbering). This would cause
;; this program to be optimized, which is not safe.

(define p (- (expt 2 31) 1))
(define A (expt 7 5))
(define x 42)

(: gen-random : (case→ (Integer Integer → Exact-Rational)
                       (Float   Float   → Float)))
(define (gen-random min max)
  (set! x (modulo (* A x) p))
  (+ min (/ (* (- max min) x) p)))

(void (gen-random 2.3 7.4))
(void (gen-random 2   7))
