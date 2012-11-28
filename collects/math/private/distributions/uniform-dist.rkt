#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide fluniform-pdf
         fluniform-cdf
         fluniform-inv-cdf
         fluniform-sample
         Uniform-Dist uniform-dist uniform-dist-min uniform-dist-max)

(: unsafe-fluniform-pdf (Float Float Float Any -> Float))
(define (unsafe-fluniform-pdf a b x log?)
  (cond [log?  (cond [(and (x . fl>= . a) (x . fl<= . b))  (- (fllog (fl- b a)))]
                     [else  -inf.0])]
        [else  (cond [(and (x . fl>= . a) (x . fl<= . b))  (fl/ 1.0 (fl- b a))]
                     [else  0.0])]))

(: unsafe-fluniform-cdf (Float Float Float Any Any -> Float))
(define (unsafe-fluniform-cdf a b x log? 1-p?)
  (cond [1-p?  (define q
                 (cond [(x . fl< . a)  1.0]
                       [(x . fl>= . b)  0.0]
                       [else  (fl/ (fl- b x) (fl- b a))]))
               (if log? (fllog q) q)]
        [else  (define q
                 (cond [(x . fl< . a)  0.0]
                       [(x . fl>= . b)  1.0]
                       [else  (fl/ (fl- x a) (fl- b a))]))
               (if log? (fllog q) q)]))

(: unsafe-fluniform-inv-cdf (Float Float Float Any Any -> Float))
(define (unsafe-fluniform-inv-cdf a b q log? 1-p?)
  (cond [(not (flprobability? q log?))  +nan.0]
        [1-p?  (let ([q  (if log? (flexp q) q)])
                 (cond [(fl= q 1.0)  a]
                       [(fl= q 0.0)  b]
                       [else  (fl+ (fl* (fl- a b) q) b)]))]
        [else  (let ([q  (if log? (flexp q) q)])
                 (cond [(fl= q 1.0)  b]
                       [(fl= q 0.0)  a]
                       [else  (fl+ (fl* (fl- b a) q) a)]))]))

(: unsafe-fluniform-sample (Float Float Natural -> FlVector))
;; Chooses a random flonum in [a,b] in a way that preserves the precision of the random flonum
;; returned by (random)
(define (unsafe-fluniform-sample a b n)
  (define d (fl- b a))
  (cond
    ;; Both positive, `a' smaller in magnitude
    [(a . fl>= . 0.0)  (build-flvector n (λ (_) (fl+ a (fl* d (random)))))]
    ;; Both negative, `b' smaller in magnitude
    [(b . fl<= . 0.0)  (build-flvector n (λ (_) (fl- b (fl* d (random)))))]
    ;; Straddle 0 case
    [else
     (build-flvector
      n (λ (_) (if ((fl* d (random)) . fl> . b) (fl* a (random)) (fl* b (random)))))]))

(begin-encourage-inline
  
  (: fluniform-pdf (Float Float Float Any -> Float))
  (define (fluniform-pdf a b x log?)
    (unsafe-fluniform-pdf (flmin a b) (flmax a b) x log?))
  
  (: fluniform-cdf (Float Float Float Any Any -> Float))
  (define (fluniform-cdf a b x log? 1-p?)
    (unsafe-fluniform-cdf (flmin a b) (flmax a b) x log? 1-p?))
  
  (: fluniform-inv-cdf (Float Float Float Any Any -> Float))
  (define (fluniform-inv-cdf a b q log? 1-p?)
    (unsafe-fluniform-inv-cdf (flmin a b) (flmax a b) q log? 1-p?))
  
  (: fluniform-sample (Float Float Integer -> FlVector))
  (define (fluniform-sample a b n)
    (cond [(n . < . 0)  (raise-argument-error 'fluniform-sample "Natural" 2 a b n)]
          [else  (unsafe-fluniform-sample (flmin a b) (flmax a b) n)]))
  
  )

;; ===================================================================================================
;; Distribution object

(define-real-dist: uniform-dist Uniform-Dist
  uniform-dist-struct ([min : Float] [max : Float]))

(begin-encourage-inline
  
  (: uniform-dist (case-> (-> Uniform-Dist)
                          (Real -> Uniform-Dist)
                          (Real Real -> Uniform-Dist)))
  (define uniform-dist
    (case-lambda
      [()   (uniform-dist 0.0 1.0)]
      [(b)  (uniform-dist 0.0 b)]
      [(a b)
       (let ([a  (fl a)] [b  (fl b)])
         (let ([a  (flmin a b)] [b  (flmax a b)])
           (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                         (unsafe-fluniform-pdf a b (fl x) log?)))
           (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                         (unsafe-fluniform-cdf a b (fl x) log? 1-p?)))
           (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                             (unsafe-fluniform-inv-cdf a b (fl p) log? 1-p?)))
           (define sample (case-lambda:
                            [()  (unsafe-flvector-ref (fluniform-sample a b 1) 0)]
                            [([n : Integer])  (flvector->list (fluniform-sample a b n))]))
           (uniform-dist-struct pdf sample cdf inv-cdf a b (delay (fl* 0.5 (fl+ a b))) a b)))]))
  
  )
