#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../types.rkt")

(provide flunif-pdf
         flunif-cdf
         flunif-inv-cdf
         flunif-random
         unif-pdf
         unif-cdf
         unif-inv-cdf
         unif-random)

(: flunif-pdf (Float Float Float Any -> Float))
(define (flunif-pdf a b x log?)
  (let ([a  (min a b)]
        [b  (max a b)])
    (cond [log?
           (cond [(and (x . >= . a) (x . <= . b))  (- (fllog (- b a)))]
                 [else  -inf.0])]
          [else
           (cond [(and (x . >= . a) (x . <= . b))  (/ 1.0 (- b a))]
                 [else  0.0])])))

(: flunif-cdf (Float Float Float Any Any -> Float))
(define (flunif-cdf a b x log? upper-tail?)
  (let ([a  (min a b)]
        [b  (max a b)])
    (cond [upper-tail?
           (define q
             (cond [(x . < . a)  1.0]
                   [(x . > . b)  0.0]
                   [else  (/ (- b x) (- b a))]))
           (if log? (fllog q) q)]
          [else
           (define q
             (cond [(x . < . a)  0.0]
                   [(x . > . b)  1.0]
                   [else  (/ (- x a) (- b a))]))
           (if log? (fllog q) q)])))

(: flunif-inv-cdf (Float Float Float Any Any -> Float))
(define (flunif-inv-cdf a b q log? upper-tail?)
  (let ([a  (min a b)]
        [b  (max a b)])
    (cond [(not (flprobability? q log?))  +nan.0]
          [upper-tail?
           (let ([q  (if log? (exp q) q)])
             (cond [(q . = . 1.0)  a]
                   [(q . = . 0.0)  b]
                   [else  (+ (* (- a b) q) b)]))]
          [else
           (let ([q  (if log? (exp q) q)])
             (cond [(q . = . 1.0)  b]
                   [(q . = . 0.0)  a]
                   [else  (+ (* (- b a) q) a)]))])))

(: flunif-random (Float Float -> Float))
;; Chooses a random flonum in [a,b] in a way that preserves the precision of the random flonum
;; returned by (random)
(define (flunif-random a b)
  (let ([a  (min a b)]
        [b  (max a b)])
    (define d (- b a))
    (cond
      ;; Both positive, `a' smaller in magnitude
      [(a . >= . 0.0)  (+ a (* d (random)))]
      ;; Both negative, `b' smaller in magnitude
      [(b . <= . 0.0)  (+ b (* (- d) (random)))]
      ;; Straddle 0 cases
      [((- a) . < . b)
       (define r (random))
       (if ((random) . < . (/ a d)) (* a r) (* b r))]
      [else
       (define r (random))
       (if ((random) . < . (/ b d)) (* b r) (* a r))])))

(begin-encourage-inline
  
  (: unif-pdf (case-> (-> Real-Density-Function)
                      (Real -> Real-Density-Function)
                      (Real Real -> Real-Density-Function)))
  (define (unif-pdf [a 0.0] [b 1.0])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (flunif-pdf a b (real->double-flonum x) log?))
      pdf))
  
  (: unif-cdf (case-> (-> Real-Distribution-Function)
                      (Real -> Real-Distribution-Function)
                      (Real Real -> Real-Distribution-Function)))
  (define (unif-cdf [a 0.0] [b 1.0])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (flunif-cdf a b (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: unif-inv-cdf (case-> (-> Real-Distribution-Function)
                          (Real -> Real-Distribution-Function)
                          (Real Real -> Real-Distribution-Function)))
  (define (unif-inv-cdf [a 0.0] [b 1.0])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf x [log? #f] [upper-tail? #f])
        (flunif-inv-cdf a b (real->double-flonum x) log? upper-tail?))
      inv-cdf))
  
  (: unif-random (case-> (-> (-> Float))
                         (Real -> (-> Float))
                         (Real Real -> (-> Float))))
  (define (unif-random [a 0.0] [b 1.0])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)])
      (λ () (flunif-random a b))))
  
  )  ; begin-encourage-inline
