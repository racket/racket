#lang typed/racket/base

(require racket/performance-hint
         "../../types.rkt")

(provide fldelta-pdf
         fldelta-cdf
         fldelta-inv-cdf
         fldelta-random
         delta-pdf
         delta-cdf
         delta-inv-cdf
         delta-random)

(: fldelta-pdf (Float Float Any -> Float))
(define (fldelta-pdf x0 x log?)
  (cond [(= x x0)  +inf.0]
        [else  (if log? -inf.0 0.0)]))

(: fldelta-cdf (Float Float Any Any -> Float))
(define (fldelta-cdf x0 x log? upper-tail?)
  (cond [(x . < . x0)
         (cond [upper-tail?  (if log? 0.0 1.0)]
               [else  (if log? -inf.0 0.0)])]
        [else
         (cond [upper-tail?  (if log? -inf.0 0.0)]
               [else  (if log? 0.0 1.0)])]))

(: fldelta-inv-cdf (Float Float Any Any -> Float))
(define (fldelta-inv-cdf x0 q log? upper-tail?)
  (cond [(not (flprobability? q log?))  +nan.0]
        [upper-tail?
         (cond [log?  (if (q . > . -inf.0) x0 +inf.0)]
               [else  (if (q . > . 0.0) x0 +inf.0)])]
        [else
         (cond [log?  (if (q . < . 0.0) x0 +inf.0)]
               [else  (if (q . < . 1.0) x0 +inf.0)])]))

(: fldelta-random (Float -> Float))
(define (fldelta-random x0) x0)

(begin-encourage-inline
  (: delta-pdf (case-> (-> Real-Density-Function)
                       (Real -> Real-Density-Function)))
  (define (delta-pdf [x0 0.0])
    (let ([x0  (real->double-flonum x0)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (fldelta-pdf x0 (real->double-flonum x) log?))
      pdf))
  
  (: delta-cdf (case-> (-> Real-Distribution-Function)
                       (Real -> Real-Distribution-Function)))
  (define (delta-cdf [x0 0.0])
    (let ([x0  (real->double-flonum x0)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (fldelta-cdf x0 (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: delta-inv-cdf (case-> (-> Real-Distribution-Function)
                           (Real -> Real-Distribution-Function)))
  (define (delta-inv-cdf [x0 0.0])
    (let ([x0  (real->double-flonum x0)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (fldelta-inv-cdf x0 (real->double-flonum q) log? upper-tail?))
      inv-cdf))
  
  (: delta-random (case-> (-> (-> Float))
                          (Real -> (-> Float))))
  (define (delta-random [x0 0.0])
    (let ([x0  (real->double-flonum x0)])
      (λ () x0)))
  
  )  ; begin-encourage-inline
