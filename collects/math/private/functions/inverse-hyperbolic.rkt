#lang typed/racket/base

(require racket/flonum
         "../../flonum.rkt"
         "log1p.rkt"
         "sqrt1pm1.rkt")

(provide flasinh asinh
         flacosh acosh
         flatanh atanh)

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic sine

(: flasinh (Float -> Float))
(define (flasinh x)
  (cond [(x . < . 0.0)  (- (flasinh (- x)))]
        [(x . = . 0.0)  0.0]
        [(x . >= . (flsqrt (flsqrt epsilon.0)))
         (cond [(x . > . (/ 1.0 (flsqrt epsilon.0)))
                ;; Laurent series in 1/x at 0+ order from -1 to 1
                (+ (fllog (* x 2.0)) (/ 1.0 (* 4.0 x x)))]
               [(x . < . 0.5)
                ;; Standard definition, rearranged to preserve digits
                (fllog1p (+ x (flsqrt1pm1 (* x x))))]
               [else
                ;; Standard definition
                (fllog (+ x (flsqrt (+ (* x x) 1.0))))])]
        [(x . >= . (flsqrt epsilon.0))
         ;; Taylor series order 2
         (* x (+ 1.0 (* #i-1/6 x x)))]
        [else  x]))  ; Taylor series order 1

(: asinh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Single-Flonum -> Single-Flonum)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (asinh x)
  (cond [(flonum? x)  (flasinh x)]
        [(single-flonum? x)  (real->single-flonum (flasinh (real->double-flonum x)))]
        [(eqv? x 0)  0]
        [(real? x)  (flasinh (real->double-flonum x))]
        [(float-complex? x)  (log (+ x (sqrt (+ (* x x) 1.0))))]
        [else  (log (+ x (sqrt (+ (* x x) 1))))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic cosine

(: flacosh (Float -> Float))
(define (flacosh x)
  (cond [(x . < . 1.0)  +nan.0]
        [((- x 1.0) . >= . (flsqrt epsilon.0))
         (cond [(x . > . (/ 1.0 (flsqrt epsilon.0)))
                ;; Laurent series in 1/x at 0+ order from -1 to 0
                (fllog (* x 2.0))]
               [(x . < . 1.5)
                ;; Standard definition, rearranged to preserve digits when x is near 1.0
                (define y (- x 1.0))
                (fllog1p (+ y (flsqrt (+ (* y y) (* 2.0 y)))))]
               [else
                ;; Standard definition
                (fllog (+ x (flsqrt (- (* x x) 1.0))))])]
        [else
         ;; Taylor series order 2
         (define y (- x 1.0))
         (* (flsqrt (* 2.0 y))
            (+ 1.0 (/ y -12.0) (/ (* 3.0 y y) 160.0)))]))

(: acosh (case-> (One -> Zero)
                 (Float -> Float)
                 (Single-Flonum -> Single-Flonum)
                 (Real -> Number)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (acosh x)
  (cond [(flonum? x)  (flacosh x)]
        [(single-flonum? x)  (real->single-flonum (flacosh (real->double-flonum x)))]
        [(eqv? x 1)  0]
        [(and (real? x) (x . >= . 1))  (flacosh (real->double-flonum x))]
        [(float-complex? x)  (log (+ x (* (sqrt (+ x 1.0)) (sqrt (- x 1.0)))))]
        [else  (log (+ x (* (sqrt (+ x 1)) (sqrt (- x 1)))))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic tangent

(: flatanh (Float -> Float))
(define (flatanh x)
  (cond [(x . < . 0.0)  (- (flatanh (- x)))]
        [(x . = . 0.0)  0.0]
        [(x . > . 1.0)  +nan.0]
        [(x . = . 1.0)  +inf.0]
        [(x . >= . (flsqrt (flsqrt epsilon.0)))
         (cond [(x . < . 0.5)
                ;; Standard definition, rearranged to preserve digits when x is near 0.0
                (* 0.5 (- (fllog1p x) (fllog1p (- x))))]
               [else
                ;; Standard definition
                (* 0.5 (fllog (/ (+ 1.0 x) (- 1.0 x))))])]
        [(x . >= . (flsqrt epsilon.0))
         ;; Taylor series order 2
         (+ x (* #i1/3 x x x))]
        [else  x]))  ; Taylor series order 1

(: atanh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Single-Flonum -> Single-Flonum)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (atanh x)
  (cond [(flonum? x)  (flatanh x)]
        [(single-flonum? x)  (real->single-flonum (flatanh (real->double-flonum x)))]
        [(eqv? x 0)  0]
        [(real? x)  (flatanh (real->double-flonum x))]
        [(float-complex? x)  (* 0.5 (- (log (+ 1.0 x)) (log (- 1.0 x))))]
        [else  (* 1/2 (- (log (+ 1 x)) (log (- 1 x))))]))
