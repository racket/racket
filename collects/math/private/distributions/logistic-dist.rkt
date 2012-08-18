#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../constants.rkt"
         "../functions/log1p.rkt"
         "../random.rkt"
         "utils.rkt")

(provide logistic-pdf logistic-log-pdf
         logistic-cdf logistic-log-cdf
         logistic-ccdf logistic-log-ccdf
         logistic-inv-cdf
         logistic-random)

(begin-encourage-inline
  (: standard-logistic-pdf (Float -> Float))
  (define (standard-logistic-pdf x)
    (let ([x  (abs x)])
      (cond [(x . > . 40.0)  (exp (- x))]
            [else
             (define exp-x (flexp x))
             (define 1+exp-x (+ 1.0 exp-x))
             (/ exp-x 1+exp-x 1+exp-x)])))
  
  (: standard-logistic-log-pdf (Float -> Float))
  (define (standard-logistic-log-pdf x)
    (let ([x  (abs x)])
      (cond [(x . > . 40.0)  (- x)]
            [else  (- x (* 2.0 (fllog1p (flexp x))))])))
  
  (: standard-logistic-cdf (Float -> Float))
  (define (standard-logistic-cdf x)
    (cond [(x . > . 40.0)  1.0]
          [(x . < . -40.0)  (exp x)]
          [else  (/ 1.0 (+ 1.0 (exp (- x))))]))
  
  (: standard-logistic-log-cdf (Float -> Float))
  (define (standard-logistic-log-cdf x)
    (cond [(x . > . 750.0)  0.0]
          [(x . < . -40.0)  x]
          [else  (- (log1p (exp (- x))))]))
  
  (: standard-logistic-inv-cdf (Float -> Float))
  (define (standard-logistic-inv-cdf p)
    (cond [(p . > . 1.0)  +nan.0]
          [(p . = . 1.0)  +inf.0]
          [(p . > . 0.0)  (- (fllog p) (fllog1p (- p)))]
          [(p . = . 0.0)  (if (eqv? p -0.0) +inf.0 -inf.0)]
          [(p . > . -1.0)  (- (fllog1p p) (fllog (- p)))]
          [(p . = . -1.0)  -inf.0]
          [else  +nan.0]))
  
  (define logistic-pdf (location-scale-pdf standard-logistic-pdf))
  (define logistic-log-pdf (location-scale-log-pdf standard-logistic-log-pdf))
  (define logistic-cdf (location-scale-cdf standard-logistic-cdf))
  (define logistic-log-cdf (location-scale-log-cdf standard-logistic-log-cdf))
  (define logistic-ccdf (location-scale-symmetric-ccdf standard-logistic-cdf))
  (define logistic-log-ccdf (location-scale-symmetric-log-ccdf standard-logistic-log-cdf))
  (define logistic-inv-cdf (location-scale-inv-cdf standard-logistic-inv-cdf))
  
  (: logistic-random (Float Float -> (-> Float)))
  (define (logistic-random x0 s)
    (: random (-> Float))
    (define random (inv-cdf-random (logistic-inv-cdf x0 s)))
    random)
  
  )  ; begin-encourage-inline
