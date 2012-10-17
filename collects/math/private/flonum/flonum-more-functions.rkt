#lang typed/racket/base

(require "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-log.rkt")

(provide flsqrt1pm1
         flsinh flcosh fltanh
         flasinh flacosh flatanh)

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
0 <= x <= 2^-28             sinh(x) ~ x
2^-28 <= x <= 1             sinh(x) = (exp(2*x) - 1) / (2*exp(x))
1 <= x <= 22                sinh(x) = (exp(2*x) - 1) / (2*exp(x))
22 <= x <= log(+max.0)      sinh(x) ~ exp(x) / 2
log(+max) <= x <= +max.0    sinh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
|#

(: flsinh (Float -> Float))
(define (flsinh x)
  (cond [(x . fl< . 0.0)  (- (flsinh (- x)))]
        [(x . fl< . (flexpt 2.0 -28.0))  x]
        [(x . fl< . 1.0)
         (define y (flexpm1 x))
         (fl* 0.5 (fl- (fl* 2.0 y) (fl/ (fl* y y) (fl+ y 1.0))))]
        [(x . fl< . 22.0)
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
log(2)/2 <= x <= 22           cosh(x) = (exp(x) + 1/exp(x)) / 2
22 <= x <= log(+max.0)        cosh(x) ~ exp(x) / 2
log(+max.0) <= x <= +max.0    cosh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
|#

(: flcosh (Float -> Float))
(define (flcosh x)
  (let ([x  (flabs x)])
    (cond [(x . fl< . (flexpt 2.0 -26.0))  1.0]
          [(x . fl< . (fl* 0.5 (fllog 2.0)))
           (define t (flexpm1 x))
           (fl+ 1.0 (fl/ (fl* t t) (fl* 2.0 (fl+ 1.0 t))))]
         [(x . fl< . 22.0)
           (define t (flexp x))
           (fl+ (fl* 0.5 t) (fl/ 0.5 t))]
         [(x . fl< . (fllog +max.0))
          (fl* 0.5 (flexp x))]
         [else
          (define w (flexp (fl* 0.5 x)))
          (fl* (fl* 0.5 w) w)])))

;; ---------------------------------------------------------------------------------------------------
;; Hyperbolic tangent

#|
tanh(x) = sinh(x) / cosh(x)
        = (exp(x) - exp(-x)) / (exp(x) + exp(-x))
        = (exp(2*x) - 1) / (exp(2*x) + 1)
        = -i * tan(i*x)

Domain               Computation

-max.0 <= x <= 0     tanh(x) = -tanh(-x)
0 <= x <= 2^-55      tanh(x) ~ x + x^2
2^-55 <= x <= 1      tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
1 <= x <= 22         tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
22 <= x <= +max.0    tanh(x) ~ 1
|#

(: fltanh (Float -> Float))
(define (fltanh x)
  (cond [(x . fl< . 0.0)  (- (fltanh (- x)))]
        [(x . fl< . (flexpt 2.0 -55.0))
         (fl* x (fl+ 1.0 x))]
        [(x . fl< . 1.0)
         (define t (flexpm1 (fl* -2.0 x)))
         (- (fl/ t (fl+ 2.0 t)))]
        [(x . fl< . 22.0)
         (define t (flexpm1 (fl* 2.0 x)))
         (fl- 1.0 (fl/ 2.0 (fl+ 2.0 t)))]
        [(x . fl<= . +inf.0)  1.0]
        [else  +nan.0]))

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
