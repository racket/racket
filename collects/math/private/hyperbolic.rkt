#lang typed/racket/base

(require racket/flonum
         "../constants.rkt"
         "expm1.rkt")

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
  (cond [(x . < . 0.0)  (- (flsinh (- x)))]
        [(x . < . (expt 2.0 -28))  x]
        [(x . < . 1.0)
         (define y (flexpm1 x))
         (* 0.5 (- (* 2.0 y) (/ (* y y) (+ y 1.0))))]
        [(x . < . 22.0)
         (define y (flexpm1 x))
         (* 0.5 (+ y (/ y (+ y 1.0))))]
        [(x . < . (fllog +max.0))
         (* 0.5 (flexp x))]
        [else
         (define y (flexp (* 0.5 x)))
         (* 0.5 y y)]))

(: sinh (Number -> Number))
(define (sinh x)
  (cond [(real? x)
         (cond [(zero? x)  x]
               [(flonum? x)  (flsinh x)]
               [(single-flonum? x)  (real->single-flonum (flsinh (real->double-flonum x)))]
               [else  (flsinh (real->double-flonum x))])]
        [else
         (* 0-i (sin (* 0+i x)))]))

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
  (let ([x  (abs x)])
    (cond [(x . < . (expt 2.0 -26))  1.0]
          [(x . < . (* 0.5 (fllog 2.0)))
           (define t (flexpm1 x))
           (+ 1.0 (/ (* t t) (* 2.0 (+ 1.0 t))))]
         [(x . < . 22.0)
           (define t (flexp x))
           (+ (* 0.5 t) (/ 0.5 t))]
         [(x . < . (fllog +max.0))
          (* 0.5 (flexp x))]
         [else
          (define w (flexp (* 0.5 x)))
          (* 0.5 w w)])))

(: cosh (Number -> Number))
(define (cosh x)
  (cond [(real? x)
         (cond [(equal? 0 x)  1]
               [(flonum? x)  (flcosh x)]
               [(single-flonum? x)  (real->single-flonum (flcosh (real->double-flonum x)))]
               [else  (flcosh (real->double-flonum x))])]
        [else
         (cos (* 0+i x))]))

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
  (cond [(x . < . 0.0)  (- (fltanh (- x)))]
        [(x . < . (expt 2.0 -55))
         (* x (+ 1.0 x))]
        [(x . < . 1.0)
         (define t (flexpm1 (* -2.0 x)))
         (- (/ t (+ 2.0 t)))]
        [(x . < . 22.0)
         (define t (flexpm1 (* 2.0 x)))
         (- 1.0 (/ 2.0 (+ 2.0 t)))]
        [(x . <= . +inf.0)  1.0]
        [else  +nan.0]))

(: tanh (Number -> Number))
(define (tanh x)
  (cond [(real? x)
         (cond [(zero? x)  x]
               [(flonum? x)  (fltanh x)]
               [(single-flonum? x)  (real->single-flonum (fltanh (real->double-flonum x)))]
               [else  (fltanh (real->double-flonum x))])]
        [else
         (* 0-i (tan (* 0+i x)))]))
