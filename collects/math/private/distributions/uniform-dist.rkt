#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide fluniform-pdf
         fluniform-cdf
         fluniform-inv-cdf
         fluniform-random
         Uniform-Distribution uniform-dist uniform-dist? uniform-dist-min uniform-dist-max)

(: unsafe-fluniform-pdf (Float Float Float Any -> Float))
(define (unsafe-fluniform-pdf a b x log?)
  (cond [log?  (cond [(and (x . >= . a) (x . <= . b))  (- (fllog (- b a)))]
                     [else  -inf.0])]
        [else  (cond [(and (x . >= . a) (x . <= . b))  (/ 1.0 (- b a))]
                     [else  0.0])]))

(: unsafe-fluniform-cdf (Float Float Float Any Any -> Float))
(define (unsafe-fluniform-cdf a b x log? 1-p?)
  (cond [1-p?  (define q
                 (cond [(x . < . a)  1.0]
                       [(x . > . b)  0.0]
                       [else  (/ (- b x) (- b a))]))
               (if log? (fllog q) q)]
        [else  (define q
                 (cond [(x . < . a)  0.0]
                       [(x . > . b)  1.0]
                       [else  (/ (- x a) (- b a))]))
               (if log? (fllog q) q)]))

(: unsafe-fluniform-inv-cdf (Float Float Float Any Any -> Float))
(define (unsafe-fluniform-inv-cdf a b q log? 1-p?)
  (cond [(not (flprobability? q log?))  +nan.0]
        [1-p?  (let ([q  (if log? (exp q) q)])
                 (cond [(q . = . 1.0)  a]
                       [(q . = . 0.0)  b]
                       [else  (+ (* (- a b) q) b)]))]
        [else  (let ([q  (if log? (exp q) q)])
                 (cond [(q . = . 1.0)  b]
                       [(q . = . 0.0)  a]
                       [else  (+ (* (- b a) q) a)]))]))

(: unsafe-fluniform-random (Float Float -> Float))
;; Chooses a random flonum in [a,b] in a way that preserves the precision of the random flonum
;; returned by (random)
(define (unsafe-fluniform-random a b)
  (define d (- b a))
  (cond
    ;; Both positive, `a' smaller in magnitude
    [(a . >= . 0.0)  (+ a (* d (random)))]
    ;; Both negative, `b' smaller in magnitude
    [(b . <= . 0.0)  (+ b (* (- d) (random)))]
    ;; Straddle 0 case
    [else  (if ((* d (random)) . > . b) (* a (random)) (* b (random)))]))

(begin-encourage-inline
  
  (: fluniform-pdf (Float Float Float Any -> Float))
  (define (fluniform-pdf a b x log?)
    (unsafe-fluniform-pdf (min a b) (max a b) x log?))
  
  (: fluniform-cdf (Float Float Float Any Any -> Float))
  (define (fluniform-cdf a b x log? 1-p?)
    (unsafe-fluniform-cdf (min a b) (max a b) x log? 1-p?))
  
  (: fluniform-inv-cdf (Float Float Float Any Any -> Float))
  (define (fluniform-inv-cdf a b q log? 1-p?)
    (unsafe-fluniform-inv-cdf (min a b) (max a b) q log? 1-p?))
  
  (: fluniform-random (Float Float -> Float))
  (define (fluniform-random a b)
    (unsafe-fluniform-random (min a b) (max a b)))
  
  )

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: uniform-dist
    Uniform-Distribution Real-Distribution ([min : Float] [max : Float]))
  
  (: uniform-dist (case-> (-> Uniform-Distribution)
                          (Real -> Uniform-Distribution)
                          (Real Real -> Uniform-Distribution)))
  (define uniform-dist
    (case-lambda
      [()   (uniform-dist 0.0 1.0)]
      [(b)  (uniform-dist 0.0 b)]
      [(a b)
       (let ([a  (fl a)] [b  (fl b)])
         (let ([a  (min a b)] [b  (max a b)])
           (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                         (unsafe-fluniform-pdf a b (fl x) log?)))
           (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                         (unsafe-fluniform-cdf a b (fl x) log? 1-p?)))
           (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                             (unsafe-fluniform-inv-cdf a b (fl p) log? 1-p?)))
           (define (random) (fluniform-random a b))
           (make-uniform-dist pdf cdf inv-cdf random a b (delay (* 0.5 (+ a b))) a b)))]))
  
  )
