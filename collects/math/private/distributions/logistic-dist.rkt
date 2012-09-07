#lang typed/racket/base

(require racket/performance-hint
         "../../flonum.rkt"
         "../../types.rkt"
         "../functions/log1p.rkt"
         "../functions/log-arithmetic.rkt"
         "utils.rkt")

(provide fllogistic-pdf
         fllogistic-cdf
         fllogistic-inv-cdf
         fllogistic-random
         logistic-pdf
         logistic-cdf
         logistic-inv-cdf
         logistic-random)

(: fllogistic-pdf (Float Float Float Any -> Float))
(define fllogistic-pdf
  (make-symmetric-location-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (cond [log?
            (cond [(x . > . 40.0)  (- x)]
                  [else  (- x (* 2.0 (fllog1p (exp x))))])]
           [else
            (cond [(x . > . 40.0)  (exp (- x))]
                  [else  (define exp-x (exp x))
                         (define 1+exp-x (+ 1.0 exp-x))
                         (/ exp-x 1+exp-x 1+exp-x)])]))))

(: fllogistic-cdf (Float Float Float Any Any -> Float))
(define fllogistic-cdf
  (make-symmetric-location-scale-flcdf
   (λ: ([x : Float] [log? : Any])
     (cond [log?
            (cond [(x . > . 750.0)  0.0]
                  [(x . < . -40.0)  x]
                  [else  (- (fllog1p (exp (- x))))])]
           [else
            (cond [(x . > . 40.0)  1.0]
                  [(x . < . -40.0)  (exp x)]
                  [else  (/ 1.0 (+ 1.0 (exp (- x))))])]))))

(: standard-fllogistic-inv-cdf (Float Any -> Float))
(define (standard-fllogistic-inv-cdf q log?)
  (cond [log?
         (cond [(q . = . (fllog 1.0))  +inf.0]
               [(q . > . (fllog 0.0))  (- q (fllog1- q))]
               [else  -inf.0])]
        [else
         (cond [(q . = . 1.0)  +inf.0]
               [(q . > . 0.0)  (- (fllog q) (fllog1p (- q)))]
               [else  -inf.0])]))

(: fllogistic-inv-cdf (Float Float Float Any Any -> Float))
(define fllogistic-inv-cdf (make-symmetric-location-scale-flinv-cdf standard-fllogistic-inv-cdf))

(: fllogistic-random (Float Float -> Float))
(define fllogistic-random (make-symmetric-location-scale-flrandom standard-fllogistic-inv-cdf))

(begin-encourage-inline
  
  (: logistic-pdf (case-> (-> Real-Density-Function)
                          (Real -> Real-Density-Function)
                          (Real Real -> Real-Density-Function)))
  (define (logistic-pdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (fllogistic-pdf x0 s (real->double-flonum x) log?))
      pdf))
  
  (: logistic-cdf (case-> (-> Real-Distribution-Function)
                          (Real -> Real-Distribution-Function)
                          (Real Real -> Real-Distribution-Function)))
  (define (logistic-cdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (fllogistic-cdf x0 s (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: logistic-inv-cdf (case-> (-> Real-Distribution-Function)
                              (Real -> Real-Distribution-Function)
                              (Real Real -> Real-Distribution-Function)))
  (define (logistic-inv-cdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (fllogistic-inv-cdf x0 s (real->double-flonum q) log? upper-tail?))
      inv-cdf))
  
  (: logistic-random (case-> (-> (-> Float))
                             (Real -> (-> Float))
                             (Real Real -> (-> Float))))
  (define (logistic-random [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (λ () (fllogistic-random x0 s))))
  
  )  ; begin-encourage-inline
