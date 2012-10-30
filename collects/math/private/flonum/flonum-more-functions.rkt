#lang typed/racket/base

(require "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-log.rkt")

(provide flsqrt1pm1
         flsinh flcosh fltanh
         flasinh flacosh flatanh
         make-flexp/base)

;; ---------------------------------------------------------------------------------------------------
;; sqrt(1+x)-1

(: flsqrt1pm1 (Float -> Float))
(define (flsqrt1pm1 x)
  (cond [((flabs x) . fl> . 0.75)
         (fl- (flsqrt (fl+ 1.0 x)) 1.0)]
        [else
         (flexpm1 (fl* 0.5 (fllog1p x)))]))

;; ---------------------------------------------------------------------------------------------------
;; Hyperbolic sine

#|
sinh(x) = (exp(x) - exp(-x)) / 2
        = (exp(2*x) - 1) / (2*exp(x))
        = -i * sin(i*x)

Domain                      Computation

-max.0 <= x <= 0            sinh(x) = -sinh(-x)
0 <= x <= 2^-26             sinh(x) ~ x
2^-26 <= x <= 18.5          sinh(x) = (exp(2*x) - 1) / (2*exp(x))
18.5 <= x <= log(+max.0)    sinh(x) ~ exp(x) / 2
log(+max) <= x <= +max.0    sinh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
|#

(: flsinh (Float -> Float))
(define (flsinh x)
  (cond [(x . fl< . 0.0)  (- (flsinh (- x)))]
        [(x . fl< . (flexpt 2.0 -26.0))  x]
        [(x . fl< . 18.5)
         (define y (flexpm1 x))
         (fl* 0.5 (fl+ y (fl/ y (fl+ y 1.0))))]
        [(x . fl< . (fllog +max.0))
         (fl* 0.5 (flexp x))]
        [else
         (define y (flexp (fl* 0.5 x)))
         (fl* (fl* 0.5 y) y)]))

;; ---------------------------------------------------------------------------------------------------
;; Hyperbolic cosine

#|
cosh(x) = (exp(x) + exp(-x)) / 2
        = (exp(2*x) + 1) / (2*exp(x))
        = cos(i*x)

Domain                        Computation

-max.0 <= x <= 0              cosh(x) = cosh(-x)
0 <= x <= 2^-26               cosh(x) ~ 1
2^-26 <= x <= log(2)/2        cosh(x) = 1 + (exp(x) - 1)^2 / (2*exp(x))
log(2)/2 <= x <= 18.5         cosh(x) = (exp(x) + 1/exp(x)) / 2
18.5 <= x <= log(+max.0)      cosh(x) ~ exp(x) / 2
log(+max.0) <= x <= +max.0    cosh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
|#

(: flcosh (Float -> Float))
(define (flcosh x)
  (let ([x  (flabs x)])
    (cond [(x . fl< . (flexpt 2.0 -26.0))  1.0]
          [(x . fl< . (fl* 0.5 (fllog 2.0)))
           (define y (flexpm1 x))
           (fl+ 1.0 (fl/ (fl* y y) (fl* 2.0 (fl+ 1.0 y))))]
         [(x . fl< . 18.5)
           (define y (flexp x))
           (fl+ (fl* 0.5 y) (fl/ 0.5 y))]
         [(x . fl< . (fllog +max.0))
          (fl* 0.5 (flexp x))]
         [else
          (define y (flexp (fl* 0.5 x)))
          (fl* (fl* 0.5 y) y)])))

;; ---------------------------------------------------------------------------------------------------
;; Hyperbolic tangent

#|
tanh(x) = sinh(x) / cosh(x)
        = (exp(x) - exp(-x)) / (exp(x) + exp(-x))
        = (exp(2*x) - 1) / (exp(2*x) + 1)
        = -i * tan(i*x)

Domain               Computation

-max.0 <= x <= 0     tanh(x) = -tanh(-x)
0 <= x <= 1e-16      tanh(x) ~ x + x^2
1e-16 <= x <= 0.5    tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
0.5 <= x <= 19.5     tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
19.5 <= x <= +max.0  tanh(x) ~ 1
|#

(: fltanh (Float -> Float))
(define (fltanh x)
  (cond [(x . fl< . 0.0)  (- (fltanh (- x)))]
        [(x . fl< . 1e-16)
         (fl* x (fl+ 1.0 x))]
        [(x . fl< . 0.5)
         (define y (flexpm1 (fl* -2.0 x)))
         (- (fl/ y (fl+ 2.0 y)))]
        [(x . fl< . 19.5)
         (define y (flexp (fl* 2.0 x)))
         (fl/ (fl- y 1.0) (fl+ y 1.0))]
        [(x . fl<= . +inf.0)  1.0]
        [else  +nan.0]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic sine

(: flasinh (Float -> Float))
(define (flasinh x)
  (cond [(x . fl< . 0.0)  (- (flasinh (- x)))]
        [(x . fl< . 2e-8)  x]
        [(x . fl< . 0.00018)
         ;; Taylor series order 3
         (fl* x (fl+ 1.0 (fl* (fl* #i-1/6 x) x)))]
        [(x . fl< . 1.0)
         ;; Standard definition, rearranged to preserve digits
         (fllog1p (fl+ x (flsqrt1pm1 (fl* x x))))]
        [(x . fl< . 3e3)
         ;; Standard definition
         (fllog (fl+ x (flsqrt (fl+ (fl* x x) 1.0))))]
        [(x . fl< . 1e307)
         ;; Laurent series in 1/x at 0+ order from -1 to 1
         (fl+ (fllog (fl* x 2.0)) (fl/ 1.0 (fl* (fl* 4.0 x) x)))]
        [else
         ;; Laurent series, rearranged to not overflow
         (fl+ (fllog x) (fllog 2.0))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic cosine

(: flacosh (Float -> Float))
(define (flacosh x)
  (cond [(x . fl< . 1.0)  +nan.0]
        [(x . fl< . 1.5)
         ;; Standard definition, rearranged to preserve digits when x is near 1.0
         (define y (fl- x 1.0))
         (fllog1p (fl+ y (flsqrt (fl+ (fl* y y) (fl* 2.0 y)))))]
        [(x . fl< . 1e8)
         ;; Standard definition
         (fllog (fl+ x (flsqrt (fl- (fl* x x) 1.0))))]
        [(x . fl< . 1e307)
         ;; Laurent series in 1/x at 0+ order from -1 to 0
         (fllog (fl* x 2.0))]
        [else
         ;; Laurent series, rearranged to avoid overflow
         (fl+ (fllog x) (fllog 2.0))]))

;; ---------------------------------------------------------------------------------------------------
;; Inverse hyperbolic tangent

(: flatanh (Float -> Float))
(define (flatanh x)
  (cond [(x . fl< . 0.0)  (- (flatanh (- x)))]
        [(x . fl< . 1e-8)  x]
        [(x . fl< . 0.00015)
         ;; Taylor series order 2
         (fl+ x (fl* (fl* (fl* #i1/3 x) x) x))]
        [(x . fl< . 0.5)
         ;; Standard definition, rearranged to preserve digits when x is near 0.0
         (fl* 0.5 (fl- (fllog1p x) (fllog1p (- x))))]
        [(x . fl< . 1.0)
         ;; Standard definition
         (fl* 0.5 (fllog (fl/ (fl+ 1.0 x) (fl- 1.0 x))))]
        [(x . fl= . 1.0)  +inf.0]
        [else  +nan.0]))

;; ---------------------------------------------------------------------------------------------------
;; Exponential with possibly rational base

(: make-flexp/base (Positive-Exact-Rational -> (Flonum -> Flonum)))
(define (make-flexp/base b)
  (define b-hi (fl b))
  (define b-lo (fl (- (/ (inexact->exact b-hi) b) 1)))
  (cond [(fl= b-lo 0.0)  (λ: ([x : Flonum]) (flexpt b-hi x))]
        [else
         (λ: ([x : Flonum])
           (fl/ (flexpt b-hi x)
                (flexp (fl* x (fllog1p b-lo)))))]))
