#lang typed/racket/base

(require "../../flonum.rkt")

(provide sinh flsinh
         cosh flcosh
         tanh fltanh)

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

(: sinh (case-> (Zero -> Zero)
                (Float -> Float)
                (Real -> Real)
                (Float-Complex -> Float-Complex)
                (Number -> Number)))
(define (sinh x)
  (cond [(real? x)
         (cond [(flonum? x)  (flsinh x)]
               [(eqv? x 0)  0]
               [else  (flsinh (fl x))])]
        [else
         (cond [(float-complex? x)  (* 0.0-1.0i (sin (* 0.0+1.0i x)))]
               [else  (* -i (sin (* +i x)))])]))

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

(: cosh (case-> (Zero -> One)
                (Float -> Float)
                (Real -> Real)
                (Float-Complex -> Float-Complex)
                (Number -> Number)))
(define (cosh x)
  (cond [(real? x)
         (cond [(flonum? x)  (flcosh x)]
               [(eqv? x 0)  1]
               [else  (flcosh (fl x))])]
        [else
         (cond [(float-complex? x)  (cos (* 0.0+1.0i x))]
               [else  (cos (* 0+i x))])]))

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

(: tanh (case-> (Zero -> Zero)
                (Float -> Float)
                (Real -> Real)
                (Float-Complex -> Float-Complex)
                (Number -> Number)))
(define (tanh x)
  (cond [(real? x)
         (cond [(flonum? x)  (fltanh x)]
               [(eqv? x 0)  0]
               [else  (fltanh (fl x))])]
        [else
         (cond [(float-complex? x)  (* 0.0-1.0i (tan (* 0.0+1.0i x)))]
               [else  (* 0-i (tan (* 0+i x)))])]))
