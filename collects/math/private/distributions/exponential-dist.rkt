#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../constants.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "../random.rkt"
         "utils.rkt")

(provide exp-pdf exp-log-pdf
         exp-cdf exp-log-cdf
         exp-ccdf exp-log-ccdf
         exp-inv-cdf
         exp-random)

(begin-encourage-inline
  (: standard-exp-pdf (Float -> Float))
  (define (standard-exp-pdf x)
    (cond [(x . < . 0.0)  0.0]
          [else  (flexp (- x))]))
  
  (: standard-exp-log-pdf (Float -> Float))
  (define (standard-exp-log-pdf x)
    (cond [(x . < . 0.0)  -inf.0]
          [else  (- x)]))
  
  (: standard-exp-cdf (Float -> Float))
  (define (standard-exp-cdf x)
    (cond [(x . <= . 0.0)  0.0]
          [else  (- (flexpm1 (- x)))]))
  
  (: standard-exp-ccdf (Float -> Float))
  (define (standard-exp-ccdf x)
    (cond [(x . <= . 0.0)  1.0]
          [else  (flexp (- x))]))
  
  (: standard-exp-log-cdf (Float -> Float))
  (define (standard-exp-log-cdf x)
    (cond [(x . < . (fllog 2.0))  (fllog (standard-exp-cdf x))]
          [else  (fllog1p (- (standard-exp-ccdf x)))]))
  
  (: standard-exp-log-ccdf (Float -> Float))
  (define (standard-exp-log-ccdf x)
    (cond [(x . <= . 0.0)  0.0]
          [else  (- x)]))
  
  (: standard-exp-inv-cdf (Float -> Float))
  (define (standard-exp-inv-cdf p)
    (cond [(p . > . 1.0)  +nan.0]
          [(p . > . 0.0)  (- (log1p (- p)))]
          [(p . = . 0.0)  (if (eqv? p -0.0) +inf.0 0.0)]
          [(p . >= . -1.0)  (- (log (- p)))]
          [else  +nan.0]))
  
  (define exp-pdf (scale-pdf standard-exp-pdf))
  (define exp-log-pdf (scale-log-pdf standard-exp-log-pdf))
  (define exp-cdf (scale-cdf standard-exp-cdf))
  (define exp-log-cdf (scale-log-cdf standard-exp-log-cdf))
  (define exp-ccdf (scale-ccdf standard-exp-ccdf))
  (define exp-log-ccdf (scale-log-ccdf standard-exp-log-ccdf))
  (define exp-inv-cdf (scale-inv-cdf standard-exp-inv-cdf))
  
  (: exp-random (Float -> (-> Float)))
  (define (exp-random s)
    (: random (-> Float))
    (define random (inv-cdf-random (exp-inv-cdf s)))
    random)
  )  ; begin-encourage-inline
