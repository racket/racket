#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "impl/delta-dist.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide fldelta-pdf
         fldelta-cdf
         fldelta-inv-cdf
         Delta-Dist delta-dist delta-dist?)

(begin-encourage-inline
  
  (define-distribution-type: Delta-Dist (Ordered-Dist Real Flonum)
    delta-dist ([center : Float]))
  
  (: delta-dist (case-> (-> Delta-Dist)
                        (Real -> Delta-Dist)))
  (define (delta-dist [c 0.0])
    (let ([c  (fl c)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (fldelta-pdf c (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (fldelta-cdf c (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (fldelta-inv-cdf c (fl p) log? 1-p?)))
      (define (random) c)
      (make-delta-dist pdf random cdf inv-cdf c c (delay c) c)))
  
  )
