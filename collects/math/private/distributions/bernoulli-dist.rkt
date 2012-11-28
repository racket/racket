#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flbernoulli-pdf
         flbernoulli-cdf
         flbernoulli-inv-cdf
         flbernoulli-sample
         Bernoulli-Dist bernoulli-dist bernoulli-dist-prob)

(: flbernoulli-pdf (Flonum Flonum Any -> Flonum))
(define (flbernoulli-pdf q k log?)
  (cond [(not (flprobability? q))  +nan.0]
        [log?  (cond [(fl= k 0.0)  (fllog1p (- q))]
                     [(fl= k 1.0)  (fllog q)]
                     [else  +nan.0])]
        [else  (cond [(fl= k 0.0)  (fl- 1.0 q)]
                     [(fl= k 1.0)  q]
                     [else  +nan.0])]))

(: flbernoulli-cdf (Flonum Flonum Any Any -> Flonum))
(define (flbernoulli-cdf q k log? 1-p?)
  (cond [(not (flprobability? q))  +nan.0]
        [1-p?  (cond [log?  (cond [(k . fl< . 0.0)  0.0]
                                  [(k . fl< . 1.0)  (fllog q)]
                                  [else  -inf.0])]
                     [else  (cond [(k . fl< . 0.0)  1.0]
                                  [(k . fl< . 1.0)  q]
                                  [else  0.0])])]
        [else  (cond [log?  (cond [(k . fl< . 0.0)  -inf.0]
                                  [(k . fl< . 1.0)  (fllog1p (- q))]
                                  [else  0.0])]
                     [else  (cond [(k . fl< . 0.0)  0.0]
                                  [(k . fl< . 1.0)  (- 1.0 q)]
                                  [else  1.0])])]))

(: flbernoulli-inv-cdf (Flonum Flonum Any Any -> Flonum))
(define (flbernoulli-inv-cdf q p log? 1-p?)
  (cond [(not (flprobability? q))  +nan.0]
        [1-p?  (cond [log?  (if (p . fl< . (fllog q)) 0.0 1.0)]
                     [else  (if (p . fl< . q) 0.0 1.0)])]
        [else  (cond [log?  (if (p . fl<= . (fllog1p (- q))) 0.0 1.0)]
                     [else  (if (p . fl<= . (fl- 1.0 q)) 0.0 1.0)])]))

(: flbernoulli-sample (Flonum Integer -> FlVector))
(define (flbernoulli-sample q n)
  (cond [(n . < . 0)  (raise-argument-error 'flbernoulli-sample "Natural" 1 q n)]
        [(not (flprobability? q))  (build-flvector n (λ (_) +nan.0))]
        [else  (build-flvector n (λ (_) (if ((random) . > . q) 0.0 1.0)))]))

(define-real-dist: bernoulli-dist Bernoulli-Dist
  bernoulli-dist-struct ([prob : Flonum]))

(begin-encourage-inline
  
  (: bernoulli-dist (case-> (-> Bernoulli-Dist)
                            (Real -> Bernoulli-Dist)))
  (define (bernoulli-dist [q 0.5])
    (let ([q  (fl q)])
      (define pdf (opt-lambda: ([k : Real] [log? : Any #f])
                    (flbernoulli-pdf q (fl k) log?)))
      (define cdf (opt-lambda: ([k : Real] [log? : Any #f] [1-p? : Any #f])
                    (flbernoulli-cdf q (fl k) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flbernoulli-inv-cdf q (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flbernoulli-sample q 1) 0)]
                       [([n : Integer])  (flvector->list (flbernoulli-sample q n))]))
      (bernoulli-dist-struct
       pdf sample cdf inv-cdf
       0.0 1.0 (delay (if (q . fl<= . 0.5) 0.0 1.0))
       q)))
  
  )
