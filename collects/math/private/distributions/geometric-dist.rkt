#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "../functions/log-arithmetic.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flgeometric-pdf
         flgeometric-cdf
         flgeometric-inv-cdf
         flgeometric-random
         Geometric-Distribution geometric-dist geometric-dist? geometric-dist-prob)

(define float-int-cutoff (expt 2 53))

(: flgeometric-pdf (Float Float Any -> Float))
(define (flgeometric-pdf p k log?)
  (cond [(or (p . <= . 0.0) (p . >= . 1.0))
         (cond [(= p 1.0)  (cond [(= k 0.0)  (if log? 0.0 1.0)]
                                 [else  (if log? -inf.0 0.0)])]
               [else  +nan.0])]
        [(k . < . 0.0)  (if log? -inf.0 0.0)]
        [else
         (cond [log?  (+ (fllog p) (* k (fllog1p (- p))))]
               [else  (* p (exp (fl (* k (fllog1p (- p))))))])]))

(: flgeometric-cdf (Float Float Any Any -> Float))
(define (flgeometric-cdf p k log? 1-p?)
  (cond [(or (p . <= . 0.0) (p . > . 1.0))  +nan.0]
        [(k . < . 0.0)  (cond [1-p?  (if log? 0.0 1.0)]
                              [else  (if log? -inf.0 0.0)])]
        [(p . = . 1.0)
         (cond [1-p?  (if log? -inf.0 0.0)]
               [else  (if log? 0.0 1.0)])]
        [else
         (define log-1-q (* (+ k 1.0) (fllog1p (- p))))
         (cond [1-p?  (if log? log-1-q (exp log-1-q))]
               [else  (if log? (fllog1- log-1-q) (- (flexpm1 log-1-q)))])]))

(: flgeometric-inv-cdf (Float Float Any Any -> Float))
(define (flgeometric-inv-cdf p q log? 1-p?)
  (define log-1-p (fllog1p (- p)))
  (: k (Float -> Float))
  (define (k log-1-q)
    (abs (max 0.0 (ceiling (/ (- log-1-q log-1-p) log-1-p)))))
  (cond [(or (p . <= . 0.0) (p . > . 1.0))  +nan.0]
        [(not (flprobability? q log?))  +nan.0]
        [(p . = . 1.0)  0.0]
        [1-p?  (if log? (k q) (k (fllog q)))]
        [else  (if log? (k (fllog1- q)) (k (fllog1p (- q))))]))

(: flgeometric-random (Float -> Float))
(define (flgeometric-random p)
  (flgeometric-inv-cdf p (* 0.5 (random)) #f ((random) . > . 0.5)))

(begin-encourage-inline
  
  (define-distribution-type: geometric-dist
    Geometric-Distribution Real-Distribution ([prob : Float]))
  
  (: geometric-dist (case-> (-> Geometric-Distribution)
                            (Real -> Geometric-Distribution)))
  (define (geometric-dist [p 0.5])
    (let ([p  (fl p)])
      (define pdf (opt-lambda: ([k : Real] [log? : Any #f])
                    (flgeometric-pdf p (fl k) log?)))
      (define cdf (opt-lambda: ([k : Real] [log? : Any #f] [1-p? : Any #f])
                    (flgeometric-cdf p (fl k) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([q : Real] [log? : Any #f] [1-p? : Any #f])
                        (flgeometric-inv-cdf p (fl q) log? 1-p?)))
      (define (random) (flgeometric-random p))
      (make-geometric-dist pdf cdf inv-cdf random
                           0.0 +inf.0 (delay (flgeometric-inv-cdf p 0.5 #f #f))
                           p)))
  
  )
