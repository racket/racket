#;
(
TR missed opt: case-arrow.rkt 21:2 (+ min (/ (* (- max min) x) p)) -- exact ops inside float expr -- caused by: 21:15 (- max min) (3 times)
TR missed opt: case-arrow.rkt 21:2 (+ min (/ (* (- max min) x) p)) -- all args float-arg-expr, result not Float -- caused by: 21:5 min, 21:18 max, 21:22 min, 21:27 x, 21:30 p (4 times)
)
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
