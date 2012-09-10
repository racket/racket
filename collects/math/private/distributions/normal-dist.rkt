#lang typed/racket/base

(require racket/performance-hint
         "../../flonum.rkt"
         "impl/normal-pdf.rkt"
         "impl/normal-cdf.rkt"
         "impl/normal-inv-cdf.rkt"
         "impl/normal-random.rkt"
         "utils.rkt"
         "types.rkt")

(provide flnormal-pdf
         flnormal-cdf
         flnormal-inv-cdf
         flnormal-random
         Normal-Distribution normal-dist normal-dist? normal-dist-mean normal-dist-stddev)

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
(define (flnormal-random x0 s)
  (+ x0 (* s (standard-flnormal-random))))

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: normal-dist
    Normal-Distribution Real-Distribution ([mean : Float] [stddev : Float]))
  
  (: normal-dist (case-> (-> Normal-Distribution)
                         (Real -> Normal-Distribution)
                         (Real Real -> Normal-Distribution)))
  (define (normal-dist [x0 0.0] [s 1.0])
    (let ([x0  (fl x0)] [s   (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flnormal-pdf x0 s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [upper-tail? : Any #f])
                    (flnormal-cdf x0 s (fl x) log? upper-tail?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [upper-tail? : Any #f])
                        (flnormal-inv-cdf x0 s (fl p) log? upper-tail?)))
      (define (random) (flnormal-random x0 s))
      (make-normal-dist pdf cdf inv-cdf random x0 s)))
  
  )
