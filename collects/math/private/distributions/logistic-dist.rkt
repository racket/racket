#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide fllogistic-pdf
         fllogistic-cdf
         fllogistic-inv-cdf
         fllogistic-random
         Logistic-Dist logistic-dist logistic-dist? logistic-dist-center logistic-dist-scale)

(: fllogistic-pdf (Float Float Float Any -> Float))
(define fllogistic-pdf
  (make-symmetric-location-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (cond [log?
            (cond [(x . fl> . 40.0)  (- x)]
                  [else  (fl- x (fl* 2.0 (fllog1p (exp x))))])]
           [else
            (cond [(x . fl> . 40.0)  (flexp (- x))]
                  [else  (define exp-x (flexp x))
                         (define 1+exp-x (fl+ 1.0 exp-x))
                         (fl/ (fl/ exp-x 1+exp-x) 1+exp-x)])]))))

(: fllogistic-cdf (Float Float Float Any Any -> Float))
(define fllogistic-cdf
  (make-symmetric-location-scale-flcdf
   (λ: ([x : Float] [log? : Any])
     (cond [log?
            (cond [(x . fl> . 750.0)  0.0]
                  [(x . fl< . -40.0)  x]
                  [else  (- (fllog1p (flexp (- x))))])]
           [else
            (cond [(x . fl> .  40.0)  1.0]
                  [(x . fl< . -40.0)  (flexp x)]
                  [else  (fl/ 1.0 (fl+ 1.0 (flexp (- x))))])]))))

(: standard-fllogistic-inv-cdf (Float Any -> Float))
(define (standard-fllogistic-inv-cdf q log?)
  (cond [log?
         (cond [(q . fl= . (fllog 1.0))  +inf.0]
               [(q . fl> . (fllog 0.0))  (fl- q (lg1- q))]
               [else  -inf.0])]
        [else
         (cond [(q . fl= . 1.0)  +inf.0]
               [(q . fl> . 0.0)  (fl- (fllog q) (fllog1p (- q)))]
               [else  -inf.0])]))

(: fllogistic-inv-cdf (Float Float Float Any Any -> Float))
(define fllogistic-inv-cdf (make-symmetric-location-scale-flinv-cdf standard-fllogistic-inv-cdf))

(: fllogistic-random (Float Float -> Float))
(define fllogistic-random (make-symmetric-location-scale-flrandom standard-fllogistic-inv-cdf))

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: Logistic-Dist (Ordered-Dist Real Flonum)
    logistic-dist ([center : Float] [scale : Float]))
  
  (: logistic-dist (case-> (-> Logistic-Dist)
                           (Real -> Logistic-Dist)
                           (Real Real -> Logistic-Dist)))
  (define (logistic-dist [c 0.0] [s 1.0])
    (let ([c  (fl c)] [s   (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (fllogistic-pdf c s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (fllogistic-cdf c s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (fllogistic-inv-cdf c s (fl p) log? 1-p?)))
      (define (random) (fllogistic-random c s))
      (make-logistic-dist pdf random cdf inv-cdf -inf.0 +inf.0 (delay c) c s)))
  
  )
