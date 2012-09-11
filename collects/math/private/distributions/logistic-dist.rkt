#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../functions/log1p.rkt"
         "../functions/log-arithmetic.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide fllogistic-pdf
         fllogistic-cdf
         fllogistic-inv-cdf
         fllogistic-random
         Logistic-Distribution logistic-dist logistic-dist? logistic-dist-center logistic-dist-scale)

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

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: logistic-dist
    Logistic-Distribution Real-Distribution ([center : Float] [scale : Float]))
  
  (: logistic-dist (case-> (-> Logistic-Distribution)
                           (Real -> Logistic-Distribution)
                           (Real Real -> Logistic-Distribution)))
  (define (logistic-dist [c 0.0] [s 1.0])
    (let ([c  (fl c)] [s   (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (fllogistic-pdf c s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (fllogistic-cdf c s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (fllogistic-inv-cdf c s (fl p) log? 1-p?)))
      (define (random) (fllogistic-random c s))
      (make-logistic-dist pdf cdf inv-cdf random -inf.0 +inf.0 (delay c) c s)))
  
  )
