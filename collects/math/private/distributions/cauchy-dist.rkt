#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../../constants.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flcauchy-pdf
         flcauchy-cdf
         flcauchy-inv-cdf
         flcauchy-random
         Cauchy-Distribution cauchy-dist cauchy-dist? cauchy-dist-center cauchy-dist-scale)

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

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: cauchy-dist
    Cauchy-Distribution Real-Distribution ([center : Float] [scale : Float]))

  (: cauchy-dist (case-> (-> Cauchy-Distribution)
                         (Real -> Cauchy-Distribution)
                         (Real Real -> Cauchy-Distribution)))
  (define (cauchy-dist [c 0.0] [s 1.0])
    (let ([c  (fl c)] [s   (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flcauchy-pdf c s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [complement? : Any #f])
                    (flcauchy-cdf c s (fl x) log? complement?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [complement? : Any #f])
                        (flcauchy-inv-cdf c s (fl p) log? complement?)))
      (define (random) (flcauchy-random c s))
      (make-cauchy-dist pdf cdf inv-cdf random -inf.0 +inf.0 (delay c) c s)))
  
  )
