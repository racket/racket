#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flexponential-pdf
         flexponential-cdf
         flexponential-inv-cdf
         flexponential-random
         Exponential-Dist exponential-dist exponential-dist? exponential-dist-scale)

(: flexponential-pdf (Float Float Any -> Float))
(define flexponential-pdf
  (make-one-sided-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (if log? (- x) (flexp (- x))))))

(: flexponential-cdf (Float Float Any Any -> Float))
(define flexponential-cdf
  (make-one-sided-scale-flcdf
   (λ: ([x : Float] [log? : Any] [1-p? : Any])
     (cond [1-p?  (if log? (- x) (flexp (- x)))]
           [else  (if log? (lg1- (- x)) (- (flexpm1 (- x))))]))))

(: standard-flexponential-inv-cdf (Float Any Any -> Float))
(define (standard-flexponential-inv-cdf q log? 1-p?)
  (cond [1-p?  (if log? (- q) (- (fllog q)))]
        [else  (if log? (- (lg1- q)) (- (fllog1p (- q))))]))

(: flexponential-inv-cdf (Float Float Any Any -> Float))
(define flexponential-inv-cdf (make-one-sided-scale-flinv-cdf standard-flexponential-inv-cdf))

(: flexponential-random (Float -> Float))
(define (flexponential-random s)
  (fl* s (- (fllog (random)))))

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: Exponential-Dist (Ordered-Dist Real Flonum)
    exponential-dist ([scale : Flonum]))
  
  (: exponential-dist (case-> (-> Exponential-Dist)
                      (Real -> Exponential-Dist)))
  (define (exponential-dist [s 1.0])
    (let ([s  (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flexponential-pdf s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flexponential-cdf s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flexponential-inv-cdf s (fl p) log? 1-p?)))
      (define (random) (flexponential-random s))
      (make-exponential-dist pdf random cdf inv-cdf 0.0 +inf.0 (delay (fl* s (fllog 2.0))) s)))
  
  )
