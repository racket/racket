#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../../vector.rkt"
         "../unsafe.rkt"
         "../functions/incomplete-gamma.rkt"
         (prefix-in impl: "impl/poisson-pdf.rkt")
         "impl/poisson-random.rkt"
         "normal-dist.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flpoisson-pdf
         flpoisson-cdf
         flpoisson-inv-cdf
         flpoisson-sample
         flpoisson-median
         Poisson-Dist poisson-dist poisson-dist-mean)

(: flpoisson-pdf (Flonum Flonum Any -> Flonum))
(define (flpoisson-pdf l k log?)
  (cond [(or (l . fl< . 0.0) (not (integer? k)))  +nan.0]
        [log?  (impl:flpoisson-log-pdf l k)]
        [else  (impl:flpoisson-pdf l k)]))

(: flpoisson-cdf (Flonum Flonum Any Any -> Flonum))
(define (flpoisson-cdf l k log? 1-p?)
  (cond [(l . fl< . 0.0)  +nan.0]
        [log?  (fllog-gamma-inc (flfloor (fl+ k 1.0)) l (not 1-p?) #t)]
        [else  (flgamma-inc (flfloor (fl+ k 1.0)) l (not 1-p?) #t)]))

(: flpoisson-inv-cdf (Flonum Flonum Any Any -> Flonum))
(define (flpoisson-inv-cdf l p log? 1-p?)
  (cond [(l . fl< . 0.0)  +nan.0]
        [(not (flprobability? p log?))  +nan.0]
        [(flprobability-one? p log? 1-p?)  +inf.0]
        [(flprobability-zero? p log? 1-p?)  0.0]
        [1-p?  (flfind-least-integer
                (λ: ([k : Flonum]) ((flpoisson-cdf l k log? 1-p?) . fl< . p))
                0.0 +inf.0
                (flmax 0.0 (flnormal-inv-cdf l (flsqrt l) p log? 1-p?)))]
        [else  (flfind-least-integer
                (λ: ([k : Flonum]) ((flpoisson-cdf l k log? 1-p?) . fl>= . p))
                0.0 +inf.0
                (flmax 0.0 (flnormal-inv-cdf l (flsqrt l) p log? 1-p?)))]))

(: flpoisson-median (Flonum -> Flonum))
(define (flpoisson-median l)
  (cond [(l . fl< . 0.0)  +nan.0]
        [else
         (define k (flfloor (fl+ l #i1/3)))
         (cond [(fl= k 0.0)  k]
               [else  (if ((flpoisson-cdf l (- k 1.0) #f #f) . fl< . 0.5) k (- k 1.0))])]))

(define-real-dist: poisson-dist Poisson-Dist
  poisson-dist-struct ([mean : Flonum]))

(begin-encourage-inline
  
  (: poisson-dist (case-> (-> Poisson-Dist)
                          (Real -> Poisson-Dist)))
  (define (poisson-dist [l 0.5])
    (let ([l  (fl l)])
      (define pdf (opt-lambda: ([k : Real] [log? : Any #f])
                    (flpoisson-pdf l (fl k) log?)))
      (define cdf (opt-lambda: ([k : Real] [log? : Any #f] [1-p? : Any #f])
                    (flpoisson-cdf l (fl k) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flpoisson-inv-cdf l (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flpoisson-sample l 1) 0)]
                       [([n : Integer])  (flvector->list (flpoisson-sample l n))]))
      (poisson-dist-struct pdf sample cdf inv-cdf 0.0 +inf.0 (delay (flpoisson-median l)) l)))
  
  )
