#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../constants.rkt"
         "../functions/log1p.rkt"
         "../random.rkt"
         "utils.rkt")

(provide cauchy-pdf cauchy-log-pdf
         cauchy-cdf cauchy-log-cdf
         cauchy-ccdf cauchy-log-ccdf
         cauchy-inv-cdf
         cauchy-random)

(begin-encourage-inline
  (: standard-cauchy-pdf (Float -> Float))
  (define (standard-cauchy-pdf x)
    (let ([x  (abs x)])
      (cond [(x . > . 1e100)
             ;; Avoid overflow in (* x x) by using 1/x
             (let* ([x  (/ 1.0 x)]
                    [x  (* x x)])
               (/ x (* pi.0 (+ 1.0 x))))]
            [else
             (/ 1.0 (* pi.0 (+ 1.0 (* x x))))])))
  
  (: standard-cauchy-log-pdf (Float -> Float))
  (define (standard-cauchy-log-pdf x)
    (let ([x  (abs x)])
      (cond [(x . > . 1e100)
             ;; Avoid overflow in (* x x) by using 1/x
             (let ([x  (/ 1.0 x)])
               (- (fllog1p (* x x)) (* -2.0 (fllog x)) (fllog pi.0)))]
            [else
             (- 0.0 (fllog1p (* x x)) (fllog pi.0))])))
  
  (: standard-cauchy-cdf (Float -> Float))
  (define (standard-cauchy-cdf x)
    (cond [(x . < . -1.0)  (- (/ (flatan (/ 1.0 x)) pi.0))]
          [(x . > . 1.0)   (- 1.0 (/ (flatan (/ 1.0 x)) pi.0))]
          [else  (+ 0.5 (/ (flatan x) pi.0))]))
  
  (: standard-cauchy-log-cdf (Float -> Float))
  (define (standard-cauchy-log-cdf x)
    (cond [(x . < . -1.0)  (- (fllog (flatan (/ 1.0 (- x)))) (fllog pi.0))]
          [(x . > . 1.0)   (fllog1p (/ (flatan (/ 1.0 (- x))) pi.0))]
          [else  (fllog (+ 0.5 (/ (flatan x) pi.0)))]))
  
  (: standard-cauchy-inv-cdf (Float -> Float))
  (define (standard-cauchy-inv-cdf p)
    ;; Main technique to preserve precision: get the argument to `fltan' close to 0.0, then use
    ;; trigonometric identities
    (cond [(p . > . 1.0)  +nan.0]
          [(p . > . 0.75)    (/ +1.0 (fltan (* pi.0 (- 1.0 p))))]
          [(p . > . 0.25)            (fltan (* pi.0 (- p 0.5)))]
          [(p . >= . -0.25)  (/ -1.0 (fltan (* pi.0 p)))]
          [(p . >= . -0.75)          (fltan (* pi.0 (+ p 0.5)))]
          [(p . >= . -1.0)   (/ -1.0 (fltan (* pi.0 (+ 1.0 p))))]
          [else  +nan.0]))
  
  (define cauchy-pdf (location-scale-pdf standard-cauchy-pdf))
  (define cauchy-log-pdf (location-scale-log-pdf standard-cauchy-log-pdf))
  (define cauchy-cdf (location-scale-cdf standard-cauchy-cdf))
  (define cauchy-log-cdf (location-scale-log-cdf standard-cauchy-log-cdf))
  (define cauchy-ccdf (location-scale-symmetric-ccdf standard-cauchy-cdf))
  (define cauchy-log-ccdf (location-scale-symmetric-log-ccdf standard-cauchy-log-cdf))
  (define cauchy-inv-cdf (location-scale-inv-cdf standard-cauchy-inv-cdf))
  
  (: cauchy-random (Float Float -> (-> Float)))
  (define (cauchy-random x0 s)
    (: random (-> Float))
    (define random (inv-cdf-random (cauchy-inv-cdf x0 s)))
    random)
  
  )  ; begin-encourage-inline
