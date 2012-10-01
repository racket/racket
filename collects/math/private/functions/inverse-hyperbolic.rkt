#lang typed/racket/base

(require "../../flonum.rkt")

(provide flasinh asinh
         flacosh acosh
         flatanh atanh)

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic sine

(: flasinh (Float -> Float))
(define (flasinh x)
  (cond [(x . fl< . 0.0)  (- (flasinh (- x)))]
        [(x . fl= . 0.0)  0.0]
        [(x . fl>= . (flsqrt (flsqrt epsilon.0)))
         (cond [(x . fl> . (fl/ 1.0 (flsqrt epsilon.0)))
                ;; Laurent series in 1/x at 0+ order from -1 to 1
                (fl+ (fllog (fl* x 2.0)) (fl/ 1.0 (fl* (fl* 4.0 x) x)))]
               [(x . fl< . 0.5)
                ;; Standard definition, rearranged to preserve digits
                (fllog1p (fl+ x (flsqrt1pm1 (fl* x x))))]
               [else
                ;; Standard definition
                (fllog (fl+ x (flsqrt (fl+ (fl* x x) 1.0))))])]
        [(x . fl>= . (flsqrt epsilon.0))
         ;; Taylor series order 2
         (fl* x (fl+ 1.0 (fl* (fl* #i-1/6 x) x)))]
        [else  x]))  ; Taylor series order 1

(: asinh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (asinh x)
  (cond [(flonum? x)  (flasinh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flasinh (fl x))]
        [(float-complex? x)  (log (+ x (sqrt (+ (* x x) 1.0))))]
        [else  (log (+ x (sqrt (+ (* x x) 1))))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic cosine

(: flacosh (Float -> Float))
(define (flacosh x)
  (cond [(x . fl< . 1.0)  +nan.0]
        [((fl- x 1.0) . fl>= . (flsqrt epsilon.0))
         (cond [(x . fl> . (fl/ 1.0 (flsqrt epsilon.0)))
                ;; Laurent series in 1/x at 0+ order from -1 to 0
                (fllog (fl* x 2.0))]
               [(x . fl< . 1.5)
                ;; Standard definition, rearranged to preserve digits when x is near 1.0
                (define y (fl- x 1.0))
                (fllog1p (fl+ y (flsqrt (fl+ (fl* y y) (fl* 2.0 y)))))]
               [else
                ;; Standard definition
                (fllog (fl+ x (flsqrt (fl- (fl* x x) 1.0))))])]
        [else
         ;; Taylor series order 2
         (define y (fl- x 1.0))
         (fl* (flsqrt (fl* 2.0 y))
              (fl+ (fl+ 1.0 (fl/ y -12.0))
                   (fl/ (fl* (fl* 3.0 y) y) 160.0)))]))

(: acosh (case-> (One -> Zero)
                 (Float -> Float)
                 (Real -> Number)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (acosh x)
  (cond [(flonum? x)  (flacosh x)]
        [(eqv? x 1)  0]
        [(and (real? x) (x . >= . 1))  (flacosh (fl x))]
        [(float-complex? x)  (log (+ x (* (sqrt (+ x 1.0)) (sqrt (- x 1.0)))))]
        [else  (log (+ x (* (sqrt (+ x 1)) (sqrt (- x 1)))))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic tangent

(: flatanh (Float -> Float))
(define (flatanh x)
  (cond [(x . fl< . 0.0)  (- (flatanh (- x)))]
        [(x . fl= . 0.0)  0.0]
        [(x . fl> . 1.0)  +nan.0]
        [(x . fl= . 1.0)  +inf.0]
        [(x . fl>= . (flsqrt (flsqrt epsilon.0)))
         (cond [(x . fl< . 0.5)
                ;; Standard definition, rearranged to preserve digits when x is near 0.0
                (fl* 0.5 (fl- (fllog1p x) (fllog1p (- x))))]
               [else
                ;; Standard definition
                (fl* 0.5 (fllog (fl/ (fl+ 1.0 x) (fl- 1.0 x))))])]
        [(x . fl>= . (flsqrt epsilon.0))
         ;; Taylor series order 2
         (fl+ x (fl* (fl* (fl* #i1/3 x) x) x))]
        [else  x]))  ; Taylor series order 1

(: atanh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (atanh x)
  (cond [(flonum? x)  (flatanh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flatanh (fl x))]
        [(float-complex? x)  (* 0.5 (- (log (+ 1.0 x)) (log (- 1.0 x))))]
        [else  (* 1/2 (- (log (+ 1 x)) (log (- 1 x))))]))
