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
         Delta-Dist delta-dist delta-dist-mean)

(define-real-dist: delta-dist Delta-Dist
  delta-dist-struct ([mean : Flonum]))

(begin-encourage-inline
  
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
      (define sample (case-lambda:
                       [()  c]
                       [([n : Integer])  (build-list n (λ (_) c))]))
      (delta-dist-struct pdf sample cdf inv-cdf c c (delay c) c)))
  
  )
