#lang typed/racket/base

(require racket/performance-hint
         "../../types.rkt"
         "../../constants.rkt"
         "../../flonum.rkt"
         "impl/normal-pdf.rkt"
         "impl/normal-cdf.rkt"
         "impl/normal-inv-cdf.rkt"
         "impl/normal-random.rkt"
         "utils.rkt")

(provide flnormal-pdf
         flnormal-cdf
         flnormal-inv-cdf
         flnormal-random
         normal-pdf
         normal-cdf
         normal-inv-cdf
         normal-random)

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

(begin-encourage-inline
  
  (: normal-pdf (case-> (-> Real-Density-Function)
                        (Real -> Real-Density-Function)
                        (Real Real -> Real-Density-Function)))
  (define (normal-pdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (flnormal-pdf x0 s (real->double-flonum x) log?))
      pdf))
  
  (: normal-cdf (case-> (-> Real-Distribution-Function)
                        (Real -> Real-Distribution-Function)
                        (Real Real -> Real-Distribution-Function)))
  (define (normal-cdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (flnormal-cdf x0 s (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: normal-inv-cdf (case-> (-> Real-Distribution-Function)
                            (Real -> Real-Distribution-Function)
                            (Real Real -> Real-Distribution-Function)))
  (define (normal-inv-cdf [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (flnormal-inv-cdf x0 s (real->double-flonum q) log? upper-tail?))
      inv-cdf))
  
  (: normal-random (case-> (-> (-> Float))
                           (Real -> (-> Float))
                           (Real Real -> (-> Float))))
  (define (normal-random [x0 0.0] [s 1.0])
    (let ([x0  (real->double-flonum x0)]
          [s   (real->double-flonum s)])
      (λ () (flnormal-random x0 s))))
  
  )  ; begin-encourage-inline
