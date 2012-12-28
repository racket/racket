#lang typed/racket/base

(require racket/performance-hint
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-log.rkt"
         "flonum-error.rkt"
         "flvector.rkt")

(provide flsqrt1pm1
         flsinh flcosh fltanh
         flasinh flacosh flatanh
         make-flexpt flexpt+ flexpt1p)

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

(: flsinh (Float -> Float))
(define (flsinh x)
  (cond [(x . fl< . 0.0)
         ;; Odd function
         (- (flsinh (- x)))]
        [(x . fl< . (flexpt 2.0 -26.0))
         ;; sinh(x) ~ x
         x]
        [(x . fl< . 18.5)
         ;; sinh(x) = (exp(2*x) - 1) / (2*exp(x))
         (define y (flexpm1 x))
         (fl* 0.5 (fl+ y (fl/ y (fl+ y 1.0))))]
        [(x . fl< . (fllog +max.0))
         ;; sinh(x) ~ exp(x) / 2
         (fl* 0.5 (flexp x))]
        [else
         ;; sinh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
         (define y (flexp (fl* 0.5 x)))
         (fl* (fl* 0.5 y) y)]))

;; ---------------------------------------------------------------------------------------------------
;; Hyperbolic cosine

(: flcosh (Float -> Float))
(define (flcosh x)
  ;; cosh(x) = cosh(-x)
  (let ([x  (flabs x)])
    (cond [(x . fl< . (flexpt 2.0 -26.0))
           ;; cosh(x) ~ 1
           1.0]
          [(x . fl< . (fl* 0.5 (fllog 2.0)))
           ;; cosh(x) = 1 + (exp(x) - 1)^2 / (2*exp(x))
           (define y (flexpm1 x))
           (fl+ 1.0 (fl/ (fl* y y) (fl* 2.0 (fl+ 1.0 y))))]
          [(x . fl< . 18.5)
           ;; cosh(x) = (exp(x) + 1/exp(x)) / 2
           (define y (flexp x))
           (fl+ (fl* 0.5 y) (fl/ 0.5 y))]
          [(x . fl< . (fllog +max.0))
           ;; cosh(x) ~ exp(x) / 2
           (fl* 0.5 (flexp x))]
          [else
           ;; cosh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
           (define y (flexp (fl* 0.5 x)))
           (fl* (fl* 0.5 y) y)])))

;; ---------------------------------------------------------------------------------------------------
;; Hyperbolic tangent

(: fltanh (Float -> Float))
(define (fltanh x)
  (cond [(x . fl< . 0.0)
         ;; tanh(x) = -tanh(-x)
         (- (fltanh (- x)))]
        [(x . fl< . 1e-16)
         ;; tanh(x) ~ x + x^2
         (fl* x (fl+ 1.0 x))]
        [(x . fl< . 0.5)
         ;; tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
         (define y (flexpm1 (fl* -2.0 x)))
         (- (fl/ y (fl+ 2.0 y)))]
        [(x . fl< . 19.5)
         ;; tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
         (define y (flexp (fl* 2.0 x)))
         (fl/ (fl- y 1.0) (fl+ y 1.0))]
        [(x . fl<= . +inf.0)
         ;; tanh(x) ~ 1
         1.0]
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
;; Exponential with high-precision bases

(begin-encourage-inline
  
  (: make-flexpt (Positive-Exact-Rational -> (Flonum -> Flonum)))
  (define (make-flexpt b)
    (define b-hi (fl b))
    (define b-lo (fl (- (/ (inexact->exact b-hi) b) 1)))
    (cond [(fl= b-lo 0.0)  (λ: ([x : Flonum]) (flexpt b-hi x))]
          [else
           (λ: ([x : Flonum])
             (fl/ (flexpt b-hi x)
                  (flexp (fl* x (fllog1p b-lo)))))]))

  (: flexpt+ (Flonum Flonum Flonum -> Flonum))
  (define (flexpt+ a b y)
    (define-values (x-hi x-lo) (fast-fl+/error a b))
    (fl/ (flexpt x-hi y)
         (flexp (fl* y (fllog1p (- (/ x-lo x-hi)))))))
  
  (: flexpt1p (Flonum Flonum -> Flonum))
  (define (flexpt1p x y)
    (cond [(and (x . > . -0.5) (x . < . +inf.0))
           (define-values (a-hi a-lo) (fast-fl+/error 1.0 x))
           (fl/ (flexpt a-hi y)
                (flexp (fl* y (fllog1p (- (/ a-lo a-hi))))))]
          [else  (flexpt (+ 1.0 x) y)]))
  
  )  ; begin-encourage-inline
