#lang typed/racket/base

(require racket/performance-hint
         "../../flonum.rkt"
         "fldelta-dist.rkt"
         "utils.rkt"
         "types.rkt")

(provide fldelta-pdf
         fldelta-cdf
         fldelta-inv-cdf
         fldelta-random
         Delta-Distribution delta-dist delta-dist? delta-dist-center)

(begin-encourage-inline
  
  (define-distribution-type: delta-dist
    Delta-Distribution Real-Distribution ([center : Float]))
  
  (: delta-dist (case-> (-> Delta-Distribution)
                        (Real -> Delta-Distribution)))
  (define (delta-dist [x0 0.0])
    (let ([x0  (fl x0)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (fldelta-pdf x0 (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [upper-tail? : Any #f])
                    (fldelta-cdf x0 (fl x) log? upper-tail?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [upper-tail? : Any #f])
                        (fldelta-inv-cdf x0 (fl p) log? upper-tail?)))
      (define (random) x0)
      (make-delta-dist pdf cdf inv-cdf random x0)))
  
  )
