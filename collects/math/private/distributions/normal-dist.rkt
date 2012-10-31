#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "impl/normal-pdf.rkt"
         "impl/normal-cdf.rkt"
         "impl/normal-inv-cdf.rkt"
         "impl/normal-random.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flnormal-pdf
         flnormal-cdf
         flnormal-inv-cdf
         flnormal-random
         Normal-Dist normal-dist normal-dist? normal-dist-mean normal-dist-stddev)

(: flnormal-pdf (Float Float Float Any -> Float))
(define flnormal-pdf
  (make-symmetric-location-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (cond [log?  (standard-flnormal-log-pdf x)]
           [else  (standard-flnormal-pdf x)]))))

(: flnormal-cdf (Float Float Float Any Any -> Float))
(define flnormal-cdf
  (make-symmetric-location-scale-flcdf
   (λ: ([x : Float] [log? : Any])
     (cond [log?  (standard-flnormal-log-cdf x)]
           [else  (standard-flnormal-cdf x)]))))

(: flnormal-inv-cdf (Float Float Float Any Any -> Float))
(define flnormal-inv-cdf
  (make-symmetric-location-scale-flinv-cdf
   (λ: ([q : Float] [log? : Any])
     (cond [log?  (standard-flnormal-inv-log-cdf q)]
           [else  (standard-flnormal-inv-cdf q)]))))

(: flnormal-random (Float Float -> Float))
(define (flnormal-random c s)
  (fl+ c (fl* s (standard-flnormal-random))))

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: Normal-Dist (Ordered-Dist Real Flonum)
    normal-dist ([mean : Float] [stddev : Float]))
  
  (: normal-dist (case-> (-> Normal-Dist)
                         (Real -> Normal-Dist)
                         (Real Real -> Normal-Dist)))
  (define (normal-dist [c 0.0] [s 1.0])
    (let ([c  (fl c)] [s   (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flnormal-pdf c s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flnormal-cdf c s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flnormal-inv-cdf c s (fl p) log? 1-p?)))
      (define (random) (flnormal-random c s))
      (make-normal-dist pdf random cdf inv-cdf -inf.0 +inf.0 (delay c) c s)))
  
  )
