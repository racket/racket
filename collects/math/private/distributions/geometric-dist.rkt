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
(define (flgeometric-pdf q k log?)
  (cond [(or (q . <= . 0.0) (q . >= . 1.0))
         (cond [(= q 1.0)  (cond [(= k 0.0)  (if log? 0.0 1.0)]
                                 [else  (if log? -inf.0 0.0)])]
               [else  +nan.0])]
        [(k . < . 0.0)  (if log? -inf.0 0.0)]
        [(integer? k)  (cond [log?  (+ (fllog q) (* k (fllog1p (- q))))]
                             [else  (* q (exp (fl (* k (fllog1p (- q))))))])]
        [else  (if log? -inf.0 0.0)]))

(: flgeometric-cdf (Float Float Any Any -> Float))
(define (flgeometric-cdf q k log? 1-p?)
  (cond [(or (q . <= . 0.0) (q . > . 1.0))  +nan.0]
        [(k . < . 0.0)  (cond [1-p?  (if log? 0.0 1.0)]
                              [else  (if log? -inf.0 0.0)])]
        [(q . = . 1.0)  (cond [1-p?  (if log? -inf.0 0.0)]
                              [else  (if log? 0.0 1.0)])]
        [else
         (define log-1-q (* (+ (floor k) 1.0) (fllog1p (- q))))
         (cond [1-p?  (if log? log-1-q (exp log-1-q))]
               [else  (if log? (fllog1- log-1-q) (- (flexpm1 log-1-q)))])]))

(: flgeometric-inv-cdf (Float Float Any Any -> Float))
(define (flgeometric-inv-cdf q p log? 1-p?)
  (cond [(or (q . <= . 0.0) (q . > . 1.0))  +nan.0]
        [(not (flprobability? p log?))  +nan.0]
        [(q . = . 1.0)  0.0]
        [else
         (define log-1-q (fllog1p (- q)))
         (define log-1-p
           (cond [1-p?  (if log? p (fllog p))]
                 [else  (if log? (fllog1- p) (fllog1p (- p)))]))
         (abs (max 0.0 (ceiling (/ (- log-1-p log-1-q) log-1-q))))]))

(: flgeometric-random (Float -> Float))
(define (flgeometric-random q)
  (flgeometric-inv-cdf q (* 0.5 (random)) #f ((random) . > . 0.5)))

(begin-encourage-inline
  
  (define-distribution-type: geometric-dist
    Geometric-Distribution Real-Distribution ([prob : Float]))
  
  (: geometric-dist (case-> (-> Geometric-Distribution)
                            (Real -> Geometric-Distribution)))
  (define (geometric-dist [q 0.5])
    (let ([q  (fl q)])
      (define pdf (opt-lambda: ([k : Real] [log? : Any #f])
                    (flgeometric-pdf q (fl k) log?)))
      (define cdf (opt-lambda: ([k : Real] [log? : Any #f] [1-p? : Any #f])
                    (flgeometric-cdf q (fl k) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flgeometric-inv-cdf q (fl p) log? 1-p?)))
      (define (random) (flgeometric-random q))
      (make-geometric-dist pdf cdf inv-cdf random
                           0.0 +inf.0 (delay (flgeometric-inv-cdf q 0.5 #f #f))
                           q)))
  
  )
