#lang typed/racket/base

(require racket/flonum
         racket/performance-hint)

(provide unif-pdf unif-log-pdf
         unif-cdf unif-log-cdf
         unif-ccdf unif-log-ccdf
         unif-inv-cdf
         unif-random)

(begin-encourage-inline
  (: unif-pdf (Float Float -> (Float -> Float)))
  (define (unif-pdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define p (/ 1.0 (- b a)))
      (λ: ([x : Float])
        (cond [(and (x . >= . a) (x . <= . b))  p]
              [else  0.0]))))
  
  (: unif-log-pdf (Float Float -> (Float -> Float)))
  (define (unif-log-pdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define log-p (- (fllog (- b a))))
      (λ: ([x : Float])
        (cond [(and (x . >= . a) (x . <= . b))  log-p]
              [else  0.0]))))
  
  (: unif-cdf (Float Float -> (Float -> Float)))
  (define (unif-cdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define d (- b a))
      (λ: ([x : Float])
        (cond [(x . < . a)  0.0]
              [(x . > . b)  1.0]
              [else  (/ (- x a) d)]))))
  
  (: unif-log-cdf (Float Float -> (Float -> Float)))
  (define (unif-log-cdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define log-d (fllog (- b a)))
      (λ: ([x : Float])
        (cond [(x . < . a)  -inf.0]
              [(x . > . b)  0.0]
              [else  (- (fllog (- x a)) log-d)]))))
  
  (: unif-ccdf (Float Float -> (Float -> Float)))
  (define (unif-ccdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define d (- b a))
      (λ: ([x : Float])
        (cond [(x . < . a)  1.0]
              [(x . > . b)  0.0]
              [else  (/ (- b x) d)]))))
  
  (: unif-log-ccdf (Float Float -> (Float -> Float)))
  (define (unif-log-ccdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define log-d (fllog (- b a)))
      (λ: ([x : Float])
        (cond [(x . < . a)  0.0]
              [(x . > . b)  -inf.0]
              [else  (- (fllog (- b x)) log-d)]))))
  
  (: unif-inv-cdf (Float Float -> (Float -> Float)))
  (define (unif-inv-cdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define d (- b a))
      (λ: ([p : Float])
        (cond [(p . > . 0.0)
               (cond [(p . > . 1.0)  +nan.0]
                     [(p . = . 1.0)  b]
                     [else  (+ (* d p) a)])]
              [(p . < . 0.0)
               (cond [(p . < . -1.0)  +nan.0]
                     [(p . = . -1.0)  a]
                     [else  (+ (* d p) b)])]
              [(eqv? p 0.0)  a]
              [else  b]))))
  
  (: unif-random (Float Float -> (-> Float)))
  ;; Chooses a random flonum in [a,b] in a way that preserves the precision of the random flonum
  ;; returned by (random)
  (define (unif-random a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define d (- b a))
      (cond
        ;; Both positive, `a' smaller in magnitude
        [(a . >= . 0.0)  (λ () (+ a (* d (random))))]
        ;; Both negative, `b' smaller in magnitude
        [(b . <= . 0.0)  (λ () (+ b (* (- d) (random))))]
        ;; Straddle 0 cases
        [((- a) . < . b)
         (λ ()
           (define r (random))
           (if ((random) . < . (/ a d)) (* a r) (* b r)))]
        [else
         (λ ()
           (define r (random))
           (if ((random) . < . (/ b d)) (* b r) (* a r)))])))
  )  ; begin-encourage-inline
