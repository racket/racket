#;#;
#<<END
TR info: case-arrow.rkt 15:12 (* (- max min) x) -- possible exact real arith
TR info: case-arrow.rkt 15:15 (- max min) -- possible exact real arith
TR info: case-arrow.rkt 15:2 (+ min (/ (* (- max min) x) p)) -- possible exact real arith
TR info: case-arrow.rkt 15:9 (/ (* (- max min) x) p) -- possible exact real arith
TR missed opt: case-arrow.rkt 15:12 (* (- max min) x) -- all args float-arg-expr, result not Float -- caused by: 15:15 (- max min), 15:27 x
TR missed opt: case-arrow.rkt 15:15 (- max min) -- all args float-arg-expr, result not Float -- caused by: 15:18 max, 15:22 min
TR missed opt: case-arrow.rkt 15:2 (+ min (/ (* (- max min) x) p)) -- all args float-arg-expr, result not Float -- caused by: 15:5 min, 15:9 (/ (* (- max min) x) p)
TR missed opt: case-arrow.rkt 15:9 (/ (* (- max min) x) p) -- all args float-arg-expr, result not Float -- caused by: 15:12 (* (- max min) x), 15:30 p
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

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
