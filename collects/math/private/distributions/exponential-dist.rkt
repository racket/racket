#lang typed/racket/base

(require racket/performance-hint
         "../../flonum.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "../functions/log-arithmetic.rkt"
         "utils.rkt"
         "types.rkt")

(provide flexp-pdf
         flexp-cdf
         flexp-inv-cdf
         flexp-random
         Exponential-Distribution exp-dist exp-dist? exp-dist-scale)

(: flexp-pdf (Float Float Any -> Float))
(define flexp-pdf
  (make-one-sided-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (if log? (- x) (exp (- x))))))

(: flexp-cdf (Float Float Any Any -> Float))
(define flexp-cdf
  (make-one-sided-scale-flcdf
   (λ: ([x : Float] [log? : Any] [upper-tail? : Any])
     (cond [upper-tail?  (if log? (- x) (exp (- x)))]
           [else  (if log? (fllog1- (- x)) (- (flexpm1 (- x))))]))))

(: standard-flexp-inv-cdf (Float Any Any -> Float))
(define (standard-flexp-inv-cdf q log? upper-tail?)
  (cond [upper-tail?  (if log? (- q) (- (fllog q)))]
        [else  (if log? (- (fllog1- q)) (- (fllog1p (- q))))]))

(: flexp-inv-cdf (Float Float Any Any -> Float))
(define flexp-inv-cdf (make-one-sided-scale-flinv-cdf standard-flexp-inv-cdf))

(: flexp-random (Float -> Float))
(define (flexp-random s)
  (* s (- (fllog (random)))))

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: exp-dist
    Exponential-Distribution Real-Distribution ([scale : Float]))
  
  (: exp-dist (case-> (-> Exponential-Distribution)
                      (Real -> Exponential-Distribution)))
  (define (exp-dist [s 1.0])
    (let ([s  (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flexp-pdf s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [upper-tail? : Any #f])
                    (flexp-cdf s (fl x) log? upper-tail?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [upper-tail? : Any #f])
                        (flexp-inv-cdf s (fl p) log? upper-tail?)))
      (define (random) (flexp-random s))
      (make-exp-dist pdf cdf inv-cdf random s)))
  
  )
