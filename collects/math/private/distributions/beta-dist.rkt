#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../functions/beta.rkt"
         "../functions/incomplete-beta.rkt"
         "impl/beta-pdf.rkt"
         "impl/beta-inv-cdf.rkt"
         "impl/gamma-random.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flbeta-pdf
         flbeta-cdf
         flbeta-inv-cdf
         flbeta-random
         Beta-Dist beta-dist beta-dist? beta-dist-alpha beta-dist-beta)

(: flbeta-pdf (Flonum Flonum Flonum Any -> Flonum))
(define (flbeta-pdf a b x log?)
  (define d (flbeta-log-pdf a b x))
  (if log? d (flexp d)))

(: flbeta-cdf (Flonum Flonum Flonum Any Any -> Flonum))
(define (flbeta-cdf a b x log? 1-p?)
  (cond [(or (a . fl< . 0.0) (b . fl< . 0.0))  +nan.0]
        [(x . fl< . 0.0)  (cond [1-p?  (if log? 0.0 1.0)]
                                [else  (if log? -inf.0 0.0)])]
        [(x . fl> . 1.0)  (cond [1-p?  (if log? -inf.0 0.0)]
                                [else  (if log? 0.0 1.0)])]
        [log?  (fllog-beta-inc a b x 1-p? #t)]
        [else  (flbeta-inc a b x 1-p? #t)]))

(: flbeta-random (Flonum Flonum -> Flonum))
(define (flbeta-random a b)
  (define x (standard-flgamma-random a))
  (define y (standard-flgamma-random b))
  (fl/ x (fl+ x y)))

(begin-encourage-inline
  
  (define-distribution-type: Beta-Dist (Ordered-Dist Real Flonum)
    beta-dist ([alpha : Flonum] [beta : Flonum]))
  
  (: beta-dist (case-> (-> Beta-Dist)
                       (Real Real -> Beta-Dist)))
  (define beta-dist
    (case-lambda
      [()  (beta-dist 1.0 1.0)]
      [(a b)
       (let ([a  (fl a)] [b  (fl b)])
         (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                       (flbeta-pdf a b (fl x) log?)))
         (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                       (flbeta-cdf a b (fl x) log? 1-p?)))
         (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                           (flbeta-inv-cdf a b (fl p) log? 1-p?)))
         (define (random) (flbeta-random a b))
         (make-beta-dist pdf random cdf inv-cdf
                         0.0 1.0 (delay (flbeta-inv-cdf a b 0.5 #f #f))
                         a b))]))
  
  )
