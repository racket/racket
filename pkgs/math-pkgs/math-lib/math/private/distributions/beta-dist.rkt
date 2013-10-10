#lang typed/racket/base

(require racket/fixnum
         racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
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
         flbeta-sample
         Beta-Dist beta-dist beta-dist-alpha beta-dist-beta)

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

(: flbeta-sample (Flonum Flonum Integer -> FlVector))
(define (flbeta-sample a b n)
  (cond [(n . < . 0)  (raise-argument-error 'flbeta-sample "Natural" 2 a b n)]
        [(or (a . fl< . 0.0) (b . fl< . 0.0)
             (and (fl= a 0.0) (fl= b 0.0))
             (and (fl= a +inf.0) (fl= b +inf.0)))
         (build-flvector n (λ (_) +nan.0))]
        [else
         (define xs (flgamma-sample a 1.0 n))
         (define ys (flgamma-sample b 1.0 n))
         (for ([i  (in-range n)])
           (define x (unsafe-flvector-ref xs i))
           (define y (unsafe-flvector-ref ys i))
           (unsafe-flvector-set! xs i (fl/ x (fl+ x y))))
         xs]))

(define-real-dist: beta-dist Beta-Dist
  beta-dist-struct ([alpha : Flonum] [beta : Flonum]))

(begin-encourage-inline
  
  (: beta-dist (Real Real -> Beta-Dist))
  (define (beta-dist a b)
    (let ([a  (fl a)] [b  (fl b)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flbeta-pdf a b (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flbeta-cdf a b (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flbeta-inv-cdf a b (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flbeta-sample a b 1) 0)]
                       [([n : Integer])  (flvector->list (flbeta-sample a b n))]))
      (beta-dist-struct
       pdf sample cdf inv-cdf
       0.0 1.0 (delay (flbeta-inv-cdf a b 0.5 #f #f))
       a b)))
  
  )
