#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../functions/log-arithmetic.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide Truncated-Distribution
         truncated-dist
         truncated-dist?
         truncated-dist-min
         truncated-dist-max
         truncated-dist-original)

(define-distribution-type: truncated-dist
  Truncated-Distribution Real-Distribution
  ([original : Real-Distribution] [min : Float] [max : Float]))

(: truncated-dist (case-> (Real-Distribution -> Truncated-Distribution)
                          (Real-Distribution Real -> Truncated-Distribution)
                          (Real-Distribution Real Real -> Truncated-Distribution)))
(define truncated-dist
  (case-lambda
    [(d)
     (cond [(truncated-dist? d)  d]
           [else
            (define a (real-dist-min d))
            (define b (real-dist-max d))
            (make-truncated-dist
             (real-dist-pdf d) (real-dist-cdf d) (real-dist-inv-cdf d) (real-dist-random d)
             (real-dist-min d) (real-dist-max d) (delay (real-dist-median d))
             d a b)])]
    [(d a)  (truncated-dist d -inf.0 a)]
    [(d a b)
     (let*-values ([(a b)  (values (fl a) (fl b))]
                   [(a b)  (values (max (real-dist-min d) (min a b))
                                   (min (real-dist-max d) (max a b)))])
       (cond [(truncated-dist? d)  (unsafe-truncated-dist (truncated-dist-original d) a b)]
             [else  (unsafe-truncated-dist d a b)]))]))

(define-syntax-rule (make-inv-cdf-random inv-cdf)
  (λ () (inv-cdf (* 0.5 (random)) #f ((random) . > . 0.5))))

(: unsafe-truncated-dist (Real-Distribution Float Float -> Truncated-Distribution))
(define (unsafe-truncated-dist d a b)
  (define dist-pdf (real-dist-pdf d))
  (define dist-cdf (real-dist-cdf d))
  (define dist-inv-cdf (real-dist-inv-cdf d))
  (define dist-random (real-dist-random d))
  (define log-P_a<x<=b (real-dist-prob d a b #t #f))
  (define log-P_x<=a (delay (dist-cdf a #t #f)))
  (define log-P_x>b (delay (dist-cdf b #t #t)))
  
  (: pdf Real-Density-Function)
  (define (pdf x [log? #f])
    (define log-d
      (cond [(x . < . a)  -inf.0]
            [(x . > . b)  -inf.0]
            [else  (- (dist-pdf x #t) log-P_a<x<=b)]))
    (if log? log-d (exp log-d)))
  
  (: cdf Real-Distribution-Function)
  (define (cdf x [log? #f] [1-p? #f])
    (let ([x  (fl x)])
      (define log-p
        (cond [1-p?  (cond [(x . < . a)  0.0]
                           [(x . > . b)  -inf.0]
                           [else  (min 0.0 (- (fllog- (dist-cdf x #t #t)
                                                      (force log-P_x>b))
                                              log-P_a<x<=b))])]
              [else  (cond [(x . < . a)  -inf.0]
                           [(x . > . b)  0.0]
                           [else  (min 0.0 (- (fllog- (dist-cdf x #t #f)
                                                      (force log-P_x<=a))
                                              log-P_a<x<=b))])]))
      (if log? log-p (exp log-p))))
  
  (: inv-cdf Real-Distribution-Function)
  (define (inv-cdf p [log? #f] [1-p? #f])
    (let ([log-p  (if log? (fl p) (fllog (fl p)))])
      (cond
        [(not (flprobability? log-p #t))  +nan.0]
        [else
         (define x
           (cond [1-p?  (cond [(= log-p 0.0)  a]
                              [(= log-p -inf.0)  b]
                              [else  (dist-inv-cdf (fllog+ (+ log-p log-P_a<x<=b)
                                                           (force log-P_x>b))
                                                   #t #t)])]
                 [else  (cond [(= log-p 0.0)  b]
                              [(= log-p -inf.0)  a]
                              [else  (dist-inv-cdf (fllog+ (+ log-p log-P_a<x<=b)
                                                           (force log-P_x<=a))
                                                   #t #f)])]))
         (min b (max a x))])))
  
  (: random (-> Float))
  (define random
    (cond [(log-P_a<x<=b . < . (fllog #i1/3))
           (make-inv-cdf-random inv-cdf)]
          [else
           (define (random)
             (define x (dist-random))
             (cond [(and (a . <= . x) (x . <= . b))  x]
                   [else  (random)]))
           random]))
  
  ;; Finally put it together
  (make-truncated-dist pdf cdf inv-cdf random
                       a b (delay (inv-cdf 0.5))
                       d a b))
