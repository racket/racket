#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../types.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "../functions/log-arithmetic.rkt"
         "utils.rkt")

(provide flgeom-pdf
         flgeom-cdf
         flgeom-inv-cdf
         flgeom-random
         geom-pdf
         geom-cdf
         geom-inv-cdf
         geom-random)

(: flgeom-pdf (Float Float Any -> Float))
(define (flgeom-pdf p k log?)
  (cond [(or (p . <= . 0.0) (p . >= . 1.0))
         (cond [(= p 1.0)  (cond [(= k 0.0)  (if log? 0.0 1.0)]
                                 [else  (if log? -inf.0 0.0)])]
               [else  +nan.0])]
        [(k . < . 0.0)  (if log? -inf.0 0.0)]
        [log?  (+ (fllog p) (* k (fllog1p (- p))))]
        [else  (* p (exp (* k (fllog1p (- p)))))]))

(: flgeom-cdf (Float Float Any Any -> Float))
(define (flgeom-cdf p k log? upper-tail?)
  (cond [(or (p . <= . 0.0) (p . > . 1.0))  +nan.0]
        [(k . < . 0.0)  (cond [upper-tail?  (if log? 0.0 1.0)]
                              [else  (if log? -inf.0 0.0)])]
        [(p . = . 1.0)
         (define q (if (k . >= . 0.0) 1.0 0.0))
         (cond [upper-tail?  (if log? (fllog1p (- q)) (- 1.0 q))]
               [else  (if log? (fllog q) q)])]
        [else
         (define log-1-q (* (+ k 1.0) (fllog1p (- p))))
         (cond [upper-tail?  (if log? log-1-q (exp log-1-q))]
               [else  (if log? (fllog1p (- (exp log-1-q))) (- (flexpm1 log-1-q)))])]))

(: flgeom-inv-cdf (Float Float Any Any -> Float))
(define (flgeom-inv-cdf p q log? upper-tail?)
  (define log-1-p (fllog1p (- p)))
  (: k (Float -> Float))
  (define (k log-1-q)
    (abs (max 0.0 (ceiling (/ (- log-1-q log-1-p) log-1-p)))))
  (cond [(or (p . <= . 0.0) (p . > . 1.0))  +nan.0]
        [(not (flprobability? q log?))  +nan.0]
        [(p . = . 1.0)  0.0]
        [upper-tail?  (if log? (k q) (k (fllog q)))]
        [else  (if log? (k (fllog (- (flexpm1 q)))) (k (fllog1p (- q))))]))

(: flgeom-random (Float -> Float))
(define (flgeom-random p)
  (flgeom-inv-cdf p (* 0.5 (random)) #f ((random) . > . 0.5)))

(begin-encourage-inline
  (: geom-pdf (case-> (-> Integer-Density-Function)
                      (Real -> Integer-Density-Function)))
  (define (geom-pdf [p 0.5])
    (let ([p  (real->double-flonum p)])
      (: pdf Integer-Density-Function)
      (define (pdf k [log? #f])
        (flgeom-pdf p (real->double-flonum k) log?))
      pdf))
  
  (: geom-cdf (case-> (-> Integer-Distribution-Function)
                      (Real -> Integer-Distribution-Function)))
  (define (geom-cdf [p 0.5])
    (let ([p  (real->double-flonum p)])
      (: cdf Integer-Distribution-Function)
      (define (cdf k [log? #f] [upper-tail? #f])1
        (flgeom-cdf p (real->double-flonum k) log? upper-tail?))
      cdf))
  
  (: geom-inv-cdf (case-> (-> Integer-Inverse-Distribution-Function)
                          (Real -> Integer-Inverse-Distribution-Function)))
  (define (geom-inv-cdf [p 0.5])
    (let ([p  (real->double-flonum p)])
      (: inv-cdf Integer-Inverse-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (flonum->extended-integer
         (flgeom-inv-cdf p (real->double-flonum q) log? upper-tail?)))
      inv-cdf))
  
  (: geom-random (Real -> (-> Extended-Integer)))
  (define (geom-random p)
    (let ([p  (real->double-flonum p)])
      (λ () (flonum->extended-integer (flgeom-random p)))))
  
  )  ; begin-encourage-inline
