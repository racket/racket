#lang typed/racket/base

(require racket/performance-hint
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt")

(provide fllog1p lg* lg/ lg1+ lg+ lg1- lg- fllog-quotient)

(begin-encourage-inline
  
  (: fllog1p (Float -> Float))
  ;; Computes the value of log(1+x) in a way that is accurate for small x
  (define (fllog1p x)
    (define ax (flabs x))
    (cond [(ax . fl>= . 1.0)  (fllog (fl+ 1.0 x))]
          [(ax . fl>= . (fl* 0.5 epsilon.0))
           (define y (fl+ 1.0 x))
           (fl- (fllog y) (fl/ (fl- (fl- y 1.0) x) y))]
          [else  x]))
  
  (: lg* (Float Float -> Float))
  (define (lg* log-x log-y) (fl+ log-x log-y))
  
  (: lg/ (Float Float -> Float))
  (define (lg/ log-x log-y) (fl- log-x log-y))
  
  (: lg1+ (Float -> Float))
  (define (lg1+ log-x)
    (cond [(log-x . fl>= . 0.0)  (fl+ log-x (fllog1p (flexp (- log-x))))]
          [else  (fllog1p (flexp log-x))]))
  
  (: lg+ (Float Float -> Float))
  (define (lg+ log-x log-y)
    (let ([log-x  (flmax log-x log-y)]
          [log-y  (flmin log-x log-y)])
      (cond [(fl= log-x -inf.0)  -inf.0]
            [else  (fl+ log-x (fllog1p (flexp (fl- log-y log-x))))])))
  
  (: lg1- (Float -> Float))
  (define (lg1- log-x)
    (cond [(log-x . fl> . (fllog 0.5))  (fllog (- (flexpm1 log-x)))]
          [else  (fllog1p (- (flexp log-x)))]))
  
  (: lg- (Float Float -> Float))
  (define (lg- log-x log-y)
    (cond [(log-y . fl> . log-x)  +nan.0]
          [else  (fl+ log-x (lg1- (fl- log-y log-x)))]))
  
  (: fllog-quotient (Flonum Flonum -> Flonum))
  ;; Computes (fllog (/ x y)) in a way that reduces error and avoids under-/overflow
  (define (fllog-quotient x y)
    (let ([x  (flabs x)]
          [y  (flabs y)]
          [s  (fl/ (flsgn x) (flsgn y))])
      (cond [(s . fl> . 0.0)
             (define z (fl/ x y))
             (cond [(and (z . fl> . +max-subnormal.0) (z . fl< . +inf.0))  (fllog (fl* s z))]
                   [else  (fl+ (fllog x) (- (fllog y)))])]
            [(s . fl= . 0.0)  -inf.0]
            [else  +nan.0])))
  
  )  ; begin-encourage-inline
