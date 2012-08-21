#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../types.rkt"
         "../../constants.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "utils.rkt")

(provide flcauchy-pdf
         flcauchy-cdf
         flcauchy-inv-cdf
         flcauchy-random
         cauchy-pdf
         cauchy-cdf
         cauchy-inv-cdf
         cauchy-random)

(: flcauchy-pdf (Float Float Float Any -> Float))
(define flcauchy-pdf
  (make-symmetric-location-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (cond [(x . > . 1e100)
            ;; Avoid overflow in (* x x) by using 1/x
            (define 1/x (/ 1.0 x))
            (define 1/x^2 (* 1/x 1/x))
            (cond [log?  (- (fllog 1/x^2) (fllog1p 1/x^2) (fllog pi.0))]
                  [else  (/ 1/x^2 (* pi.0 (+ 1.0 1/x^2)))])]
           [else
            (cond [log?  (- (- (fllog1p (* x x))) (fllog pi.0))]
                  [else  (/ 1.0 (* pi.0 (+ 1.0 (* x x))))])]))))

(: flcauchy-cdf (Float Float Float Any Any -> Float))
(define flcauchy-cdf
  (make-symmetric-location-scale-flcdf
   (λ: ([x : Float] [log? : Any])
     ;; Main technique to preserve precision: keep the argument to `flatan' smaller than 1
     (cond [log?  (cond [(x . < . -1.0)  (- (fllog (flatan (/ 1.0 (- x)))) (fllog pi.0))]
                        [(x . > . 1.0)   (fllog1p (/ (flatan (/ 1.0 (- x))) pi.0))]
                        [else  (fllog (+ 0.5 (/ (flatan x) pi.0)))])]
           [else  (cond [(x . < . -1.0)  (- (/ (flatan (/ 1.0 x)) pi.0))]
                        [(x . > . 1.0)   (- 1.0 (/ (flatan (/ 1.0 x)) pi.0))]
                        [else  (+ 0.5 (/ (flatan x) pi.0))])]))))

(: standard-flcauchy-inv-cdf (Float Any -> Float))
(define (standard-flcauchy-inv-cdf q log?)
  ;; Main technique to preserve precision: get the argument to `fltan' close to 0.0
  (cond [log?
         (cond [(q . > . (log 0.75))  (/ +1.0 (fltan (* pi.0 (- (flexpm1 q)))))]
               [(q . > . (log 0.25))          (fltan (* pi.0 (- (exp q) 0.5)))]
               [else                  (/ -1.0 (fltan (* pi.0 (exp q))))])]
        [else
         (cond [(q . > . 0.75)    (/ +1.0 (fltan (* pi.0 (- 1.0 q))))]
               [(q . > . 0.25)            (fltan (* pi.0 (- q 0.5)))]
               [else              (/ -1.0 (fltan (* pi.0 q)))])]))

(: flcauchy-inv-cdf (Float Float Float Any Any -> Float))
(define flcauchy-inv-cdf (make-symmetric-location-scale-flinv-cdf standard-flcauchy-inv-cdf))

(: flcauchy-random (Float Float -> Float))
(define flcauchy-random (make-symmetric-location-scale-flrandom standard-flcauchy-inv-cdf))

(begin-encourage-inline
  
  (: cauchy-pdf (case-> (-> Real-Density-Function)
                        (Real -> Real-Density-Function)
                        (Real Real -> Real-Density-Function)))
  (define (cauchy-pdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (flcauchy-pdf x0 s (real->double-flonum x) log?))
      pdf))
  
  (: cauchy-cdf (case-> (-> Real-Distribution-Function)
                        (Real -> Real-Distribution-Function)
                        (Real Real -> Real-Distribution-Function)))
  (define (cauchy-cdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (flcauchy-cdf x0 s (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: cauchy-inv-cdf (case-> (-> Real-Distribution-Function)
                            (Real -> Real-Distribution-Function)
                            (Real Real -> Real-Distribution-Function)))
  (define (cauchy-inv-cdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (flcauchy-inv-cdf x0 s (real->double-flonum q) log? upper-tail?))
      inv-cdf))
  
  (: cauchy-random (case-> (-> (-> Float))
                           (Real -> (-> Float))
                           (Real Real -> (-> Float))))
  (define (cauchy-random [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (λ () (flcauchy-random x0 s))))
  
  )  ; begin-encourage-inline
