#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
         "../functions/incomplete-beta.rkt"
         (prefix-in impl: "impl/binomial-pdf.rkt")
         "impl/binomial-random.rkt"
         "normal-dist.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flbinomial-pdf
         flbinomial-cdf
         flbinomial-inv-cdf
         flbinomial-sample
         Binomial-Dist binomial-dist binomial-dist-count binomial-dist-prob)

(: flbinomial-pdf (Flonum Flonum Flonum Any -> Flonum))
(define (flbinomial-pdf n q k log?)
  (cond [log?  (impl:flbinomial-log-pdf n q k)]
        [else  (impl:flbinomial-pdf n q k)]))

(: flbinomial-cdf (Flonum Flonum Flonum Any Any -> Flonum))
(define (flbinomial-cdf n q k log? 1-p?)
  (cond [(or (not (flinteger? n)) (n . fl< . 0.0)
             (q . fl< . 0.0) (q . fl> . 1.0))
         +nan.0]
        [else
         (let ([k  (flfloor k)])
           (cond [log?  (fllog-beta-inc (fl+ k 1.0) (fl- n k) q (not 1-p?) #t)]
                 [else  (flbeta-inc (fl+ k 1.0) (fl- n k) q (not 1-p?) #t)]))]))

(: flbinomial-inv-cdf (Flonum Flonum Flonum Any Any -> Flonum))
(define (flbinomial-inv-cdf n q p log? 1-p?)
  (cond [(or (not (flinteger? n)) (n . fl< . 0.0)
             (q . fl< . 0.0) (q . fl> . 1.0)
             (not (flprobability? p log?)))
         +nan.0]
        [(flprobability-one? p log? 1-p?)  n]
        [(flprobability-zero? p log? 1-p?)  0.0]
        [1-p?
         (define z (flnormal-inv-cdf (fl* n q) (flsqrt (* n q (fl- 1.0 q))) p log? #t))
         (flfind-least-integer
          (λ: ([k : Flonum]) ((flbinomial-cdf n q k log? 1-p?) . fl< . p))
          0.0 n
          (flmax 0.0 (flmin n z)))]
        [else
         (define z (flnormal-inv-cdf (fl* n q) (flsqrt (* n q (fl- 1.0 q))) p log? #f))
         (flfind-least-integer
          (λ: ([k : Flonum]) ((flbinomial-cdf n q k log? 1-p?) . fl>= . p))
          0.0 n
          (flmax 0.0 (flmin n z)))]))

(define-real-dist: binomial-dist Binomial-Dist
  binomial-dist-struct ([count : Flonum] [prob : Flonum]))

(begin-encourage-inline
  
  (: binomial-dist (case-> (-> Binomial-Dist)
                           (Real -> Binomial-Dist)
                           (Real Real -> Binomial-Dist)))
  (define (binomial-dist [n 1.0] [q 0.5])
    (let ([n  (fl n)] [q  (fl q)])
      (define pdf (opt-lambda: ([k : Real] [log? : Any #f])
                    (flbinomial-pdf n q (fl k) log?)))
      (define cdf (opt-lambda: ([k : Real] [log? : Any #f] [1-p? : Any #f])
                    (flbinomial-cdf n q (fl k) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flbinomial-inv-cdf n q (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flbinomial-sample n q 1) 0)]
                       [([m : Integer])  (flvector->list (flbinomial-sample n q m))]))
      (binomial-dist-struct
       pdf sample cdf inv-cdf
       0.0 +inf.0 (delay (flfloor (fl* n q)))
       n q)))
  
  )
