#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flgeometric-pdf
         flgeometric-cdf
         flgeometric-inv-cdf
         flgeometric-sample
         Geometric-Dist geometric-dist geometric-dist-prob)

(: flgeometric-pdf (Flonum Flonum Any -> Flonum))
(define (flgeometric-pdf q k log?)
  (cond [(not (flinteger? k))  +nan.0]
        [(or (q . fl<= . 0.0) (q . fl>= . 1.0))
         (cond [(fl= q 1.0)  (cond [(fl= k 0.0)  (if log? 0.0 1.0)]
                                   [else  (if log? -inf.0 0.0)])]
               [(fl= q 0.0)  (if log? -inf.0 0.0)]
               [else  +nan.0])]
        [(k . fl< . 0.0)  (if log? -inf.0 0.0)]
        [log?  (fl+ (fllog q) (fl* k (fllog1p (- q))))]
        [else  (fl* q (flexp (fl* k (fllog1p (- q)))))]))

(: flgeometric-cdf (Flonum Flonum Any Any -> Flonum))
(define (flgeometric-cdf q k log? 1-p?)
  (cond [(or (q . fl<= . 0.0) (q . fl>= . 1.0))
         (cond [(fl= q 1.0)  (flprobability (if (k . fl>= . 0.0) 1.0 0.0) log? 1-p?)]
               [(fl= q 0.0)  (flprobability (if (k . fl= . +inf.0) 1.0 0.0) log? 1-p?)]
               [else  +nan.0])]
        [(k . fl< . 0.0)  (flprobability 0.0 log? 1-p?)]
        [else
         (let ([k  (flfloor k)])
           (define log-1-p (fl* (fl+ k 1.0) (fllog1p (- q))))
           (cond [1-p?  (if log? log-1-p (exp log-1-p))]
                 [else  (if log? (lg1- log-1-p) (- (flexpm1 log-1-p)))]))]))

(: flgeometric-inv-cdf (Flonum Flonum Any Any -> Flonum))
(define (flgeometric-inv-cdf q p log? 1-p?)
  (cond [(not (flprobability? p log?))  +nan.0]
        [(or (q . fl<= . 0.0) (q . fl>= . 1.0))
         (cond [(fl= q 1.0)  0.0]
               [(fl= q 0.0)  (if (flprobability-zero? p log? 1-p?) 0.0 +inf.0)]
               [else  +nan.0])]
        [else
         (define log-1-p
           (cond [1-p?  (if log? p (fllog p))]
                 [else  (if log? (lg1- p) (fllog1p (- p)))]))
         (flmax 0.0 (fl- (flceiling (fl/ log-1-p (fllog1p (- q)))) 1.0))]))

(: flgeometric-sample (Flonum Integer -> FlVector))
(define (flgeometric-sample q n)
  (cond [(n . < . 0)  (raise-argument-error 'flgeometric-sample "Natural" 1 q n)]
        [(or (q . fl<= . 0.0) (q . fl>= . 1.0))
         (define v
           (cond [(fl= q 1.0)  0.0]
                 [(fl= q 0.0)  +inf.0]
                 [else  +nan.0]))
         (build-flvector n (λ (_) v))]
        [else
         (build-flvector
          n (λ (_)
              (define p (fl* 0.5 (random)))
              (define log-1-p (if ((random) . fl> . 0.5) (fllog p) (fllog1p (- p))))
              (flmax 0.0 (fl- (flceiling (fl/ log-1-p (fllog1p (- q)))) 1.0))))]))

(define-real-dist: geometric-dist Geometric-Dist
  geometric-dist-struct ([prob : Flonum]))

(begin-encourage-inline
  
  (: geometric-dist (case-> (-> Geometric-Dist)
                            (Real -> Geometric-Dist)))
  (define (geometric-dist [q 0.5])
    (let ([q  (fl q)])
      (define pdf (opt-lambda: ([k : Real] [log? : Any #f])
                    (flgeometric-pdf q (fl k) log?)))
      (define cdf (opt-lambda: ([k : Real] [log? : Any #f] [1-p? : Any #f])
                    (flgeometric-cdf q (fl k) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flgeometric-inv-cdf q (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flgeometric-sample q 1) 0)]
                       [([n : Integer])  (flvector->list (flgeometric-sample q n))]))
      (geometric-dist-struct
       pdf sample cdf inv-cdf
       0.0 +inf.0 (delay (flgeometric-inv-cdf q 0.5 #f #f))
       q)))
  
  )
