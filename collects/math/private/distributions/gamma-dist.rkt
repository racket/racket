#lang typed/racket/base

(require racket/performance-hint
         "../../flonum.rkt"
         "../functions/incomplete-gamma.rkt"
         "impl/gamma-pdf.rkt"
         "impl/gamma-inv-cdf.rkt"
         "impl/gamma-random.rkt"
         "utils.rkt"
         "types.rkt")

(provide flgamma-pdf
         flgamma-cdf
         flgamma-inv-cdf
         flgamma-random
         Gamma-Distribution gamma-dist gamma-dist? gamma-dist-shape gamma-dist-scale)

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

(: flgamma-inv-cdf (Float Float Float Any Any -> Float))
(define (flgamma-inv-cdf k s x log? upper-tail?)
  ((make-one-sided-scale-flinv-cdf
    (λ: ([p : Float] [log? : Any] [upper-tail? : Any])
      (standard-flgamma-inv-cdf k p log? upper-tail?)))
   s x log? upper-tail?))

(: flgamma-random (Float Float -> Float))
(define (flgamma-random k s)
  (* s (standard-flgamma-random k)))

(begin-encourage-inline
  
  (define-distribution-type: gamma-dist
    Gamma-Distribution Real-Distribution ([shape : Float] [scale : Float]))
  
  (: gamma-dist (case-> (-> Gamma-Distribution)
                        (Real -> Gamma-Distribution)
                        (Real Real -> Gamma-Distribution)))
  (define (gamma-dist [k 1.0] [s 1.0])
    (let ([k  (fl k)] [s  (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flgamma-pdf k s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [upper-tail? : Any #f])
                    (flgamma-cdf k s (fl x) log? upper-tail?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [upper-tail? : Any #f])
                        (flgamma-inv-cdf k s (fl p) log? upper-tail?)))
      (define (random) (flgamma-random k s))
      (make-gamma-dist pdf cdf inv-cdf random k s)))
  
  )
