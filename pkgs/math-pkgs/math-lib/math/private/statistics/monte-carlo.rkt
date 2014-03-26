#lang typed/racket

(require "../../base.rkt"
         "../distributions/beta-dist.rkt"
         "expected-values.rkt"
         "statistics-utils.rkt")

(provide mc-variance
         mc-variance/mean
         mc-stddev
         mc-stddev/mean
         indicator
         mc-probability
         mc-prob-dist)

;; ---------------------------------------------------------------------------------------------------
;; Monte Carlo variance and standard deviation

;; Correct MC variance for ratio importance method computation is from notes on a lecture series
;; Petri Koistinen. Monte Carlo Methods, with an emphasis on Bayesian computation. Summer 2010

(: mc-variance* (-> Symbol Real (Sequenceof Real) (Option (Sequenceof Real)) (U Boolean Real)
                    Nonnegative-Real))
(define (mc-variance* name m xs ws bias)
  (define-values (xs^2 n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples 'mc-variance xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (sqr (* w (- x m)))) xs ws)
                         (max 0 (sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (sqr (- x m))) xs)
                           (length xs)))]))
  (cond [(zero? n)  +nan.0]
        [else
         (define m2 (max 0 (/ (sum xs^2) (sqr n))))
         (adjust-variance m2 n bias)]))

(: mc-variance/mean (->* [Real (Sequenceof Real)]
                         [(Option (Sequenceof Real)) #:bias (U Boolean Real)]
                         Nonnegative-Real))
(define (mc-variance/mean m xs [ws #f] #:bias [bias #f])
  (mc-variance* 'mc-variance/mean m xs ws bias))

(: mc-variance (->* [(Sequenceof Real)]
                    [(Option (Sequenceof Real)) #:bias (U Boolean Real)]
                    Nonnegative-Real))
(define (mc-variance xs [ws #f] #:bias [bias #f])
  (mc-variance* 'mc-variance (mean xs ws) xs ws bias))

(: mc-stddev/mean (->* [Real (Sequenceof Real)]
                       [(Option (Sequenceof Real)) #:bias (U Boolean Real)]
                       Nonnegative-Real))
(define (mc-stddev/mean m xs [ws #f] #:bias [bias #f])
  (sqrt (mc-variance* 'mc-stddev/mean m xs ws bias)))

(: mc-stddev (->* [(Sequenceof Real)]
                  [(Option (Sequenceof Real)) #:bias (U Boolean Real)]
                  Nonnegative-Real))
(define (mc-stddev xs [ws #f] #:bias [bias #f])
  (sqrt (mc-variance* 'mc-stddev (mean xs ws) xs ws bias)))

;; ---------------------------------------------------------------------------------------------------
;; Monte Carlo probabilities

(: indicator (All (A) ((A -> Any) -> (A -> (U 0 1)))))
(define ((indicator f) x) (if (f x) 1 0))

(: mc-probability (All (A) (->* [(A -> Any) (Sequenceof A)]
                                [(Option (Sequenceof Real))]
                                Nonnegative-Real)))
(define (mc-probability p? xs [ws #f])
  (let-values ([(xs ws)  (cond [ws    (sequences->weighted-samples 'mc-prob xs ws)]
                               [else  (values (sequence->list xs) #f)])])
    (max 0 (mean (map ((inst indicator A) p?) xs) ws))))

(: mc-prob-dist (All (A) (->* [(A -> Any) (Sequenceof A)]
                              [(Option (Sequenceof Real))]
                              Beta-Dist)))
(define (mc-prob-dist p? xs [ws #f])
  (let-values ([(xs ws)  (cond [ws    (sequences->weighted-samples 'mc-prob-dist xs ws)]
                               [else  (values (sequence->list xs) #f)])])
    (define n (length xs))
    (define α
      (cond [ws    (* n (mean (map ((inst indicator A) p?) xs) ws))]
            [else  (count p? xs)]))
    (beta-dist α (- n α))))
