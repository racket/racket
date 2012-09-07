#lang racket

(module defs typed/racket/base
  
  (require racket/flonum
           racket/performance-hint
           "../../types.rkt"
           "../functions/expm1.rkt"
           "../functions/log1p.rkt"
           "../functions/gamma.rkt"
           "../functions/log-gamma.rkt"
           "../functions/log-arithmetic.rkt"
           "../functions/incomplete-gamma.rkt"
           "../../constants.rkt"
           "impl/gamma-pdf.rkt"
           "utils.rkt")
  
  (provide (all-defined-out))
  #;
  (provide flgamma-pdf
           flgamma-cdf
           flgamma-inv-cdf
           flgamma-random
           gamma-pdf
           gamma-cdf
           gamma-inv-cdf
           gamma-random)
  
  (: flgamma-pdf (Float Float Float Any -> Float))
  (define (flgamma-pdf k s x log?)
    ((make-one-sided-scale-flpdf
      (λ: ([x : Float] [log? : Any])
        (cond [(k . <= . 0.0)  +nan.0]
              ;; Outside of support:
              [(= x 0.0)  (if log? -inf.0 0.0)]
              [log?  (standard-flgamma-log-pdf k x)]
              [else  (standard-flgamma-pdf k x)])))
     s x log?))
  
  (: flgamma-cdf (Float Float Float Any Any -> Float))
  (define (flgamma-cdf k s x log? upper-tail?)
    ((make-one-sided-scale-flcdf
      (λ: ([x : Float] [log? : Any] [upper-tail? : Any])
        (cond [upper-tail?
               (cond [log?  (fllog-gamma-upper-regularized k x)]
                     [else  (flgamma-upper-regularized k x)])]
              [else
               (cond [log?  (fllog-gamma-lower-regularized k x)]
                     [else  (flgamma-lower-regularized k x)])])))
     s x log? upper-tail?))
  
  #|
(: standard-flgamma-inv-cdf (Float Any Any -> Float))
(define (standard-flgamma-inv-cdf q log? upper-tail?)
  (cond [upper-tail?
         (cond [log?  (- q)]
               [else  (- (fllog q))])]
        [else
         (cond [log?  (- (fllog1p (- (exp q))))]
               [else  (- (fllog1p (- q)))])]))

(: flgamma-inv-cdf (Float Float Any Any -> Float))
(define flgamma-inv-cdf (make-one-sided-scale-flinv-cdf standard-flgamma-inv-cdf))

(: flgamma-random (Float -> Float))
(define flgamma-random (make-one-sided-scale-flrandom standard-flgamma-inv-cdf))
|#
  
  (begin-encourage-inline
    (: gamma-pdf (case-> (-> Real-Density-Function)
                         (Real -> Real-Density-Function)
                         (Real Real -> Real-Density-Function)))
    (define (gamma-pdf [k 1.0] [s 1.0])
      (let ([k  (real->double-flonum k)]
            [s  (real->double-flonum s)])
        (: pdf Real-Density-Function)
        (define (pdf x [log? #f])
          (flgamma-pdf k s (real->double-flonum x) log?))
        pdf))
    
    (: gamma-cdf (case-> (-> Real-Distribution-Function)
                         (Real -> Real-Distribution-Function)
                         (Real Real -> Real-Distribution-Function)))
    (define (gamma-cdf [k 1.0] [s 1.0])
      (let ([k  (real->double-flonum k)]
            [s  (real->double-flonum s)])
        (: cdf Real-Distribution-Function)
        (define (cdf x [log? #f] [upper-tail? #f])
          (flgamma-cdf k s (real->double-flonum x) log? upper-tail?))
        cdf))
    #|
    (: exp-inv-cdf (case-> (-> Real-Distribution-Function)
                           (Real -> Real-Distribution-Function)))
    (define (exp-inv-cdf [s 1.0])
      (let ([s  (real->double-flonum s)])
        (: inv-cdf Real-Distribution-Function)
        (define (inv-cdf q [log? #f] [upper-tail? #f])
          (flgamma-inv-cdf s (real->double-flonum q) log? upper-tail?))
        inv-cdf))
    
    (: exp-random (case-> (-> (-> Float))
                          (Real -> (-> Float))))
    (define (exp-random [s 1.0])
      (let ([s  (real->double-flonum s)])
        (λ () (flgamma-random s))))
    |#
    )  ; begin-encourage-inline
  )

(require 'defs plot math/flonum math/bigfloat racket/flonum
         math/private/functions/gamma
         math/private/functions/log-gamma
         math/private/functions/incomplete-gamma
         math/private/functions/log1p
         math/private/functions/expm1
         math/private/functions/log-arithmetic)

(bf-precision 128)

(define (cdf k x)
  (flgamma-cdf k 1.0 x #t #t))

(define h (make-hash))

(define (cdf* k x)
  ;(printf "k = ~v  x = ~v~n" k x)
  (hash-ref!
   h (cons k x)
   (λ ()
     (let ([k  (bf k)]
           [x  (bf x)])
       (bigfloat->flonum
        (bflog-gamma-upper-regularized k x))))))

(define k-min +min.0)
(define x-min 0.0)
(define k-max 1000)
(define x-max 1000)

(plot3d (contour-intervals3d
         (λ (k x)
           (let ([k  (real->double-flonum k)]
                 [x  (real->double-flonum x)])
             (printf "k = ~v  x = ~v~n" k x)
             (cdf* k x)))
         k-min k-max x-min x-max)
        #:x-label "k"
        #:y-label "x")

(plot3d (contour-intervals3d
         (λ (k x)
           (let ([k  (real->double-flonum k)]
                 [x  (real->double-flonum x)])
             ;(printf "k = ~v  x = ~v~n" k x)
             (cdf k x)))
         k-min k-max x-min x-max)
        #:x-label "k"
        #:y-label "x")

(plot3d (contour-intervals3d
         (λ (k x)
           (let ([k  (real->double-flonum k)]
                 [x  (real->double-flonum x)])
             (define res
               (abs (relative-error (cdf k x)
                                    (cdf* k x))))
             (if (rational? res) res -1.0)))
         k-min k-max x-min x-max)
        #:x-label "k"
        #:y-label "x")
