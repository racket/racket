#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../types.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "utils.rkt")

(provide flexp-pdf
         flexp-cdf
         flexp-inv-cdf
         flexp-random
         exp-pdf
         exp-cdf
         exp-inv-cdf
         exp-random)

(: flexp-pdf (Float Float Any -> Float))
(define flexp-pdf
  (make-one-sided-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (if log? (- x) (exp (- x))))))

(: flexp-cdf (Float Float Any Any -> Float))
(define flexp-cdf
  (make-one-sided-scale-flcdf
   (λ: ([x : Float] [log? : Any] [upper-tail? : Any])
     (cond [upper-tail?
            (cond [log?  (- x)]
                  [else  (exp (- x))])]
           [else
            (cond [log?  (fllog1p (- (exp (- x))))]
                  [else  (- (flexpm1 (- x)))])]))))

(: standard-flexp-inv-cdf (Float Any Any -> Float))
(define (standard-flexp-inv-cdf q log? upper-tail?)
  (cond [upper-tail?
         (cond [log?  (- q)]
               [else  (- (fllog q))])]
        [else
         (cond [log?  (- (fllog1p (- (exp q))))]
               [else  (- (fllog1p (- q)))])]))

(: flexp-inv-cdf (Float Float Any Any -> Float))
(define flexp-inv-cdf (make-one-sided-scale-flinv-cdf standard-flexp-inv-cdf))

(: flexp-random (Float -> Float))
(define flexp-random (make-one-sided-scale-flrandom standard-flexp-inv-cdf))

(begin-encourage-inline
  (: exp-pdf (case-> (-> Real-Density-Function)
                     (Real -> Real-Density-Function)))
  (define (exp-pdf [s 1.0])
    (let ([s  (real->double-flonum s)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (flexp-pdf s (real->double-flonum x) log?))
      pdf))
  
  (: exp-cdf (case-> (-> Real-Distribution-Function)
                     (Real -> Real-Distribution-Function)))
  (define (exp-cdf [s 1.0])
    (let ([s  (real->double-flonum s)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (flexp-cdf s (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: exp-inv-cdf (case-> (-> Real-Distribution-Function)
                         (Real -> Real-Distribution-Function)))
  (define (exp-inv-cdf [s 1.0])
    (let ([s  (real->double-flonum s)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (flexp-inv-cdf s (real->double-flonum q) log? upper-tail?))
      inv-cdf))
  
  (: exp-random (case-> (-> (-> Float))
                        (Real -> (-> Float))))
  (define (exp-random [s 1.0])
    (let ([s  (real->double-flonum s)])
      (λ () (flexp-random s))))
  
  )  ; begin-encourage-inline
