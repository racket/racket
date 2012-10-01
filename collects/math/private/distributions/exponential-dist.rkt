#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "dist-struct.rkt"
         "utils.rkt")

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
   (λ: ([x : Float] [log? : Any] [1-p? : Any])
     (cond [1-p?  (if log? (- x) (exp (- x)))]
           [else  (if log? (lg1- (- x)) (- (flexpm1 (- x))))]))))

(: standard-flexp-inv-cdf (Float Any Any -> Float))
(define (standard-flexp-inv-cdf q log? 1-p?)
  (cond [1-p?  (if log? (- q) (- (fllog q)))]
        [else  (if log? (- (lg1- q)) (- (fllog1p (- q))))]))

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
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flexp-cdf s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flexp-inv-cdf s (fl p) log? 1-p?)))
      (define (random) (flexp-random s))
      (make-exp-dist pdf cdf inv-cdf random 0.0 +inf.0 (delay (* s (fllog 2.0))) s)))
  
  )
