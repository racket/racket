#;
(
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:12 (* (- max min) x) -- all args float-arg-expr, result not Float -- caused by: 41:15 (- max min), 41:27 x
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:9 (/ (* (- max min) x) p) -- all args float-arg-expr, result not Float -- caused by: 41:12 (* (- max min) x), 41:30 p
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:12 (* (- max min) x) -- all args float-arg-expr, result not Float -- caused by: 41:15 (- max min), 41:27 x
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:2 (+ min (/ (* (- max min) x) p)) -- all args float-arg-expr, result not Float -- caused by: 41:5 min, 41:9 (/ (* (- max min) x) p)
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:12 (* (- max min) x) -- all args float-arg-expr, result not Float -- caused by: 41:15 (- max min), 41:27 x
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:9 (/ (* (- max min) x) p) -- all args float-arg-expr, result not Float -- caused by: 41:12 (* (- max min) x), 41:30 p
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
TR missed opt: case-arrow.rkt 41:12 (* (- max min) x) -- all args float-arg-expr, result not Float -- caused by: 41:15 (- max min), 41:27 x
TR missed opt: case-arrow.rkt 41:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 41:18 max, 41:22 min
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
