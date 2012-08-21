#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "functions/log1p.rkt")

(provide flprobability? probability?
         fllog* fllog/ fllog+ fllog-)

(begin-encourage-inline
  
  (: flprobability? (Float Any -> Boolean))
  (define (flprobability? p log?)
    (cond [log?  (and (p . >= . -inf.0) (p . <= . 0.0))]
          [else  (and (p . >= . 0.0) (p . <= . 1.0))]))
  
  (: probability? (case-> (Real -> Boolean)
                          (Real Any -> Boolean)))
  (define (probability? p [log? #f])
    (flprobability? (real->double-flonum p) log?))
  
  
  (: fllog* (Float Float -> Float))
  (define (fllog* log-x log-y) (+ log-x log-y))
  
  (: fllog/ (Float Float -> Float))
  (define (fllog/ log-x log-y) (- log-x log-y))
  
  (: fllog+ (Float Float -> Float))
  (define (fllog+ log-x log-y)
    (let ([log-x  (max log-x log-y)]
          [log-y  (min log-x log-y)])
      (+ log-x (fllog1p (exp (- log-y log-x))))))
  
  (: fllog- (Float Float -> Float))
  (define (fllog- log-x log-y)
    (cond [(log-y . > . log-x)  +nan.0]
          [else  (+ log-x (fllog1p (- (exp (- log-y log-x)))))]))
  
  )  ; begin-encourage-inline
