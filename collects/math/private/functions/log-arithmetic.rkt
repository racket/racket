#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "log1p.rkt"
         "expm1.rkt")

(provide fllog* fllog/ fllog1+ fllog+ fllog1- fllog-)

(begin-encourage-inline
  
  (: fllog* (Float Float -> Float))
  (define (fllog* log-x log-y) (+ log-x log-y))
  
  (: fllog/ (Float Float -> Float))
  (define (fllog/ log-x log-y) (- log-x log-y))
  
  (: fllog1+ (Float -> Float))
  (define (fllog1+ log-x)
    (cond [(log-x . >= . 0.0)  (+ log-x (fllog1p (exp (- log-x))))]
          [else  (fllog1p (exp log-x))]))
  
  (: fllog+ (Float Float -> Float))
  (define (fllog+ log-x log-y)
    (let ([log-x  (max log-x log-y)]
          [log-y  (min log-x log-y)])
      (+ log-x (fllog1p (exp (- log-y log-x))))))
  
  (: fllog1- (Float -> Float))
  (define (fllog1- log-x)
    (cond [(log-x . > . (fllog 0.5))  (fllog (- (flexpm1 log-x)))]
          [else  (fllog1p (- (exp log-x)))]))
  
  (: fllog- (Float Float -> Float))
  (define (fllog- log-x log-y)
    (cond [(log-y . > . log-x)  +nan.0]
          [else  (+ log-x (fllog1- (- log-y log-x)))]))
  
  )  ; begin-encourage-inline
