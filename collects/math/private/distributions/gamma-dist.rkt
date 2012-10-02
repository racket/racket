#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../functions/incomplete-gamma.rkt"
         "impl/gamma-pdf.rkt"
         "impl/gamma-inv-cdf.rkt"
         "impl/gamma-random.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flgamma-pdf
         flgamma-cdf
         flgamma-inv-cdf
         flgamma-random
         Gamma-Distribution gamma-dist gamma-dist? gamma-dist-shape gamma-dist-scale)

(: flgamma-pdf (Float Float Float Any -> Float))
(define (flgamma-pdf k s x log?)
  ((make-one-sided-scale-flpdf
    (λ: ([x : Float] [log? : Any])
      (cond [(k . fl<= . 0.0)  +nan.0]
            ;; Outside of support:
            [(fl= x 0.0)  (if log? -inf.0 0.0)]
            [log?  (standard-flgamma-log-pdf k x)]
            [else  (standard-flgamma-pdf k x)])))
   s x log?))

(: flgamma-cdf (Float Float Float Any Any -> Float))
(define (flgamma-cdf k s x log? 1-p?)
  ((make-one-sided-scale-flcdf
    (λ: ([x : Float] [log? : Any] [1-p? : Any])
      (cond [1-p?
             (cond [log?  (fllog-gamma-upper-regularized k x)]
                   [else  (flgamma-upper-regularized k x)])]
            [else
             (cond [log?  (fllog-gamma-lower-regularized k x)]
                   [else  (flgamma-lower-regularized k x)])])))
   s x log? 1-p?))

(: flgamma-inv-cdf (Float Float Float Any Any -> Float))
(define (flgamma-inv-cdf k s p log? 1-p?)
  ((make-one-sided-scale-flinv-cdf
    (λ: ([p : Float] [log? : Any] [1-p? : Any])
      (standard-flgamma-inv-cdf k p log? 1-p?)))
   s p log? 1-p?))

(: flgamma-random (Float Float -> Float))
(define (flgamma-random k s)
  (fl* s (standard-flgamma-random k)))

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
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flgamma-cdf k s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flgamma-inv-cdf k s (fl p) log? 1-p?)))
      (define (random) (flgamma-random k s))
      (make-gamma-dist pdf cdf inv-cdf random
                       0.0 +inf.0 (delay (flgamma-inv-cdf k s 0.5 #f #f))
                       k s)))
  
  )
