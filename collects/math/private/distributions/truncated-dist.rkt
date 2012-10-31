#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide Truncated-Dist
         truncated-dist
         truncated-dist?
         truncated-dist-min
         truncated-dist-max
         truncated-dist-original)

(define-distribution-type: Truncated-Dist (Ordered-Dist Real Flonum)
  truncated-dist ([original : Real-Dist] [min : Float] [max : Float]))

(: truncated-dist (case-> (Real-Dist -> Truncated-Dist)
                          (Real-Dist Real -> Truncated-Dist)
                          (Real-Dist Real Real -> Truncated-Dist)))
(define truncated-dist
  (case-lambda
    [(d)    (truncated-dist d -inf.0 +inf.0)]
    [(d a)  (truncated-dist d -inf.0 a)]
    [(d a b)
     (let*-values ([(a b)  (values (fl a) (fl b))]
                   [(a b)  (values (max (dist-min d) (min a b))
                                   (min (dist-max d) (max a b)))])
       (unsafe-truncated-dist d a b))]))

(define-syntax-rule (make-inv-cdf-random inv-cdf)
  (λ () (inv-cdf (fl* 0.5 (random)) #f ((random) . fl> . 0.5))))

(: unsafe-truncated-dist (Real-Dist Float Float -> Truncated-Dist))
(define (unsafe-truncated-dist d a b)
  (define orig-pdf (dist-pdf d))
  (define orig-cdf (dist-cdf d))
  (define orig-inv-cdf (dist-inv-cdf d))
  (define orig-random (dist-random d))
  (define log-P_a<x<=b (real-dist-prob d a b #t #f))
  (define log-P_x<=a (delay (orig-cdf a #t #f)))
  (define log-P_x>b (delay (orig-cdf b #t #t)))
  
  (: pdf Real-PDF)
  (define (pdf x [log? #f])
    (let ([x  (fl x)])
      (define log-d
        (cond [(x . fl< . a)  -inf.0]
              [(x . fl> . b)  -inf.0]
              [else  (fl- (orig-pdf x #t) log-P_a<x<=b)]))
      (if log? log-d (flexp log-d))))
  
  (: cdf Real-CDF)
  (define (cdf x [log? #f] [1-p? #f])
    (let ([x  (fl x)])
      (define log-p
        (cond [1-p?  (cond [(x . fl< . a)  0.0]
                           [(x . fl> . b)  -inf.0]
                           [else  (flmin 0.0 (fl- (lg- (orig-cdf x #t #t)
                                                       (force log-P_x>b))
                                                  log-P_a<x<=b))])]
              [else  (cond [(x . fl< . a)  -inf.0]
                           [(x . fl> . b)  0.0]
                           [else  (flmin 0.0 (fl- (lg- (orig-cdf x #t #f)
                                                       (force log-P_x<=a))
                                                  log-P_a<x<=b))])]))
      (if log? log-p (flexp log-p))))
  
  (: inv-cdf Real-Inverse-CDF)
  (define (inv-cdf p [log? #f] [1-p? #f])
    (let ([log-p  (if log? (fl p) (fllog (fl p)))])
      (cond
        [(not (flprobability? log-p #t))  +nan.0]
        [else
         (define x
           (cond [1-p?  (cond [(fl= log-p 0.0)  a]
                              [(fl= log-p -inf.0)  b]
                              [else  (orig-inv-cdf (lg+ (fl+ log-p log-P_a<x<=b)
                                                        (force log-P_x>b))
                                                   #t #t)])]
                 [else  (cond [(fl= log-p 0.0)  b]
                              [(fl= log-p -inf.0)  a]
                              [else  (orig-inv-cdf (lg+ (fl+ log-p log-P_a<x<=b)
                                                        (force log-P_x<=a))
                                                   #t #f)])]))
         (min b (max a x))])))
  
  (: random (-> Float))
  (define random
    (cond [(log-P_a<x<=b . fl< . (- (fllog 3.0)))
           (make-inv-cdf-random inv-cdf)]
          [else
           (define (random)
             (define x (orig-random))
             (cond [(and (a . fl<= . x) (x . fl<= . b))  x]
                   [else  (random)]))
           random]))
  
  ;; Finally put it together
  (make-truncated-dist pdf random cdf inv-cdf
                       a b (delay (inv-cdf 0.5))
                       d a b))
