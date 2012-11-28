#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
         "../functions/incomplete-gamma.rkt"
         "impl/gamma-pdf.rkt"
         "impl/gamma-inv-cdf.rkt"
         "impl/gamma-random.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flgamma-pdf
         flgamma-cdf
         flgamma-inv-cdf
         flgamma-sample
         Gamma-Dist gamma-dist gamma-dist-shape gamma-dist-scale)

(: flgamma-pdf (Float Float Float Any -> Float))
(define (flgamma-pdf k s x log?)
  (cond [(k . fl< . 0.0)  +nan.0]
        [else
         ((make-one-sided-scale-flpdf
           (λ: ([x : Float] [log? : Any])
             (cond [log?  (standard-flgamma-log-pdf k x)]
                   [else  (standard-flgamma-pdf k x)])))
          s x log?)]))

(: flgamma-cdf (Float Float Float Any Any -> Float))
(define (flgamma-cdf k s x log? 1-p?)
  (cond [(k . fl< . 0.0)  +nan.0]
        [else
         ((make-one-sided-scale-flcdf
           (λ: ([x : Float] [log? : Any] [1-p? : Any])
             (cond [log?  (fllog-gamma-inc k x 1-p? #t)]
                   [else  (flgamma-inc k x 1-p? #t)])))
          s x log? 1-p?)]))

(: flgamma-inv-cdf (Float Float Float Any Any -> Float))
(define (flgamma-inv-cdf k s p log? 1-p?)
  (cond [(k . fl< . 0.0)  +nan.0]
        [else
         ((make-one-sided-scale-flinv-cdf
           (λ: ([p : Float] [log? : Any] [1-p? : Any])
             (standard-flgamma-inv-cdf k p log? 1-p?)))
          s p log? 1-p?)]))

(define-real-dist: gamma-dist Gamma-Dist
  gamma-dist-struct ([shape : Flonum] [scale : Flonum]))

(begin-encourage-inline
  
  (: gamma-dist (case-> (-> Gamma-Dist)
                        (Real -> Gamma-Dist)
                        (Real Real -> Gamma-Dist)))
  (define (gamma-dist [k 1.0] [s 1.0])
    (let ([k  (fl k)] [s  (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flgamma-pdf k s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flgamma-cdf k s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flgamma-inv-cdf k s (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flgamma-sample k s 1) 0)]
                       [([n : Integer])  (flvector->list (flgamma-sample k s n))]))
      (gamma-dist-struct
       pdf sample cdf inv-cdf
       0.0 +inf.0 (delay (flgamma-inv-cdf k s 0.5 #f #f))
       k s)))
  
  )
