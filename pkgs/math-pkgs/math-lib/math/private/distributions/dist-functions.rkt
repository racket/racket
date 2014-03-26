#lang typed/racket/base

(require racket/promise
         "../../flonum.rkt"
         "dist-struct.rkt")

(provide real-dist-prob
         real-dist-hpd-interval)

;; ===================================================================================================
;; Computing probabilities

(: real-dist-prob* (Real-Dist Flonum Flonum Any -> Flonum))
;; Assumes a <= b
(define (real-dist-prob* d a b 1-p?)
  (define c (force (ordered-dist-median d)))
  (define cdf (ordered-dist-cdf d))
  (define p
    (cond [(a . fl= . b)  (if 1-p? 1.0 0.0)]
          [1-p?  (fl+ (cdf a #f #f) (cdf b #f #t))]
          [(b . fl<= . c)
           ;; Both less than the median; use lower tail only
           (fl- (cdf b #f #f) (cdf a #f #f))]
          [(a . fl>= . c)
           ;; Both greater than the median; use upper tail only
           (fl- (cdf a #f #t) (cdf b #f #t))]
          [else
           ;; Median between a and b; use lower for (a,c] and upper for (c,b]
           (define P_x<=a (cdf a #f #f))
           (define P_x>b (cdf b #f #t))
           (fl+ (fl- 0.5 P_x<=a) (fl- 0.5 P_x>b))]))
  (max 0.0 (min 1.0 p)))

(: real-dist-log-prob* (Real-Dist Flonum Flonum Any -> Flonum))
;; Assumes a <= b
(define (real-dist-log-prob* d a b 1-p?)
  (define c (force (ordered-dist-median d)))
  (define cdf (ordered-dist-cdf d))
  (define log-p
    (cond [(a . fl= . b)  (if 1-p? 0.0 -inf.0)]
          [1-p?  (lg+ (cdf a #t #f) (cdf b #t #t))]
          [(b . fl<= . c)
           ;; Both less than the median; use lower tail only
           (define log-P_x<=a (cdf a #t #f))
           (define log-P_x<=b (cdf b #t #f))
           (cond [(log-P_x<=b . fl< . log-P_x<=a)  -inf.0]
                 [else  (lg- log-P_x<=b log-P_x<=a)])]
          [(a . fl>= . c)
           ;; Both greater than the median; use upper tail only
           (define log-P_x>a (cdf a #t #t))
           (define log-P_x>b (cdf b #t #t))
           (cond [(log-P_x>a . fl< . log-P_x>b)  -inf.0]
                 [else  (lg- log-P_x>a log-P_x>b)])]
          [else
           ;; Median between a and b; try 1-upper first
           (define log-P_x<=a (cdf a #t #f))
           (define log-P_x>b (cdf b #t #t))
           (define log-p (lg1- (lg+ log-P_x<=a log-P_x>b)))
           (cond [(log-p . fl> . (log 0.1))  log-p]
                 [else
                  ;; Subtracting from 1.0 (in log space) lost bits; split and add instead
                  (define log-P_a<x<=c
                    (cond [((fllog 0.5) . fl< . log-P_x<=a)  -inf.0]
                          [else  (lg- (fllog 0.5) log-P_x<=a)]))
                  (define log-P_c<x<=b
                    (cond [((fllog 0.5) . fl< . log-P_x>b)  -inf.0]
                          [else  (lg- (fllog 0.5) log-P_x>b)]))
                  (lg+ log-P_a<x<=c log-P_c<x<=b)])]))
  (min 0.0 log-p))

(: real-dist-prob (case-> (Real-Dist Real Real -> Flonum)
                          (Real-Dist Real Real Any -> Flonum)
                          (Real-Dist Real Real Any Any -> Flonum)))
(define (real-dist-prob d a b [log? #f] [1-p? #f])
  (let ([a  (fl a)] [b  (fl b)])
    (let ([a  (flmin a b)] [b  (flmax a b)])
      (cond [log?  (define p (real-dist-prob* d a b 1-p?))
                   (cond [(and (p . fl> . +max-subnormal.0) (p . fl< . 0.9))  (fllog p)]
                         [else  (real-dist-log-prob* d a b 1-p?)])]
            [else  (real-dist-prob* d a b 1-p?)]))))

;; ===================================================================================================
;; Highest probability density (HPD) regions

(: real-dist-hpd-interval (-> Real-Dist Real (Values Flonum Flonum)))
(define (real-dist-hpd-interval d α)
  (when (or (α . <= . 0) (α . > . 1))
    (raise-argument-error 'real-dist-hpd-interval "Real in (0,1]" 1 d α))
  
  (let ([α  (max (* 128.0 epsilon.0) (fl α))])
    (cond
      [(α . >= . 1.0)  (values (ordered-dist-min d) (ordered-dist-max d))]
      [else
       (define pdf (distribution-pdf d))
       (define cdf (ordered-dist-cdf d))
       (define inv-cdf (ordered-dist-inv-cdf d))
       
       (: high-endpoint (-> Real Flonum))
       (define (high-endpoint a)
         (inv-cdf (min 1.0 (+ (cdf a) α))))
       
       (: objective (-> Flonum Flonum))
       (define (objective p)
         (define a (inv-cdf p))
         (define b (high-endpoint a))
         (- (pdf a) (pdf b)))
       
       (define p (flbracketed-root objective +min.0 (- 1.0 α)))
       
       (define a
         (cond [(<= 0.0 p 1.0)  (inv-cdf p)]
               [else
                (if ((abs (objective 0.0)) . < . (abs (objective (- 1.0 α))))
                    (inv-cdf 0.0)
                    (inv-cdf (- 1.0 α)))]))
       
       (values a (high-endpoint a))])))
