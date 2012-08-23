#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "log1p.rkt")

(provide fllog* fllog/ fllog+ fllog-)

(begin-encourage-inline
  
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
