#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt")

(provide
 ;; Real-valued distributions
 Real-Density-Function
 Real-Distribution-Function
 Real-Distribution
 real-dist-pdf
 real-dist-cdf
 real-dist-inv-cdf
 real-dist-random
 real-dist-min
 real-dist-max
 real-dist-median
 real-dist-prob
 random-real)

;; ===================================================================================================
;; Real-valued distributions

(define-type Real-Density-Function
  (case-> (Real -> Float)
          (Real Any -> Float)))

(define-type Real-Distribution-Function
  (case-> (Real -> Float)
          (Real Any -> Float)
          (Real Any Any -> Float)))

(struct: Real-Distribution ([pdf : Real-Density-Function]
                            [cdf : Real-Distribution-Function]
                            [inv-cdf : Real-Distribution-Function]
                            [random : (-> Float)]
                            [min : Float]
                            [max : Float]
                            [median : (Promise Float)])
  #:transparent)

(begin-encourage-inline
  
  (define real-dist-pdf Real-Distribution-pdf)
  (define real-dist-cdf Real-Distribution-cdf)
  (define real-dist-inv-cdf Real-Distribution-inv-cdf)
  (define real-dist-random Real-Distribution-random)
  
  (define real-dist-min Real-Distribution-min)
  (define real-dist-max Real-Distribution-max)
  
  (: real-dist-median (Real-Distribution -> Float))
  (define (real-dist-median d) (force (Real-Distribution-median d)))
  
  (: random-real (Real-Distribution -> Float))
  (define (random-real d) ((real-dist-random d)))
  
  )

(: real-dist-prob (case-> (Real-Distribution Real Real -> Float)
                          (Real-Distribution Real Real Any -> Float)
                          (Real-Distribution Real Real Any Any -> Float)))
(define (real-dist-prob d a b [log? #f] [1-p? #f])
  (let ([a  (fl a)] [b  (fl b)])
    (let ([a  (flmin a b)] [b  (flmax a b)])
      (define c (real-dist-median d))
      (define cdf (real-dist-cdf d))
      (define log-p
        (min (cond [1-p?  (lg+ (cdf a #t #f) (cdf b #t #t))]
                   [(b . fl<= . c)
                    (define log-P_x<=a (cdf a #t #f))
                    (define log-P_x<=b (cdf b #t #f))
                    (cond [(log-P_x<=b . fl< . log-P_x<=a)  -inf.0]
                          [else  (lg- log-P_x<=b log-P_x<=a)])]
                   [(a . fl>= . c)
                    (define log-P_x>a (cdf a #t #t))
                    (define log-P_x>b (cdf b #t #t))
                    (cond [(log-P_x>a . fl< . log-P_x>b)  -inf.0]
                          [else  (lg- log-P_x>a log-P_x>b)])]
                   [else
                    (define log-P_x<=a (cdf a #t #f))
                    (define log-P_x>b (cdf b #t #t))
                    (define log-P_a<x<=0.5
                      (cond [((fllog 0.5) . fl< . log-P_x<=a)  -inf.0]
                            [else  (lg- (fllog 0.5) log-P_x<=a)]))
                    (define log-P_0.5<x<=b
                      (cond [((fllog 0.5) . fl< . log-P_x>b)  -inf.0]
                            [else  (lg- (fllog 0.5) log-P_x>b)]))
                    (lg+ log-P_a<x<=0.5 log-P_0.5<x<=b)])
             0.0))
      (if log? log-p (flexp log-p)))))
