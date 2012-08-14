#lang typed/racket/base

(require racket/flonum
         "../../flonum.rkt")

(provide fllog1p log1p)

;; Compute the value of log(1+x) in a way that is accurate for small x

(: fllog1p (Float -> Float))
(define (fllog1p x)
  (define ax (abs x))
  (cond [(ax . >= . 1.0)  (fllog (+ 1.0 x))]
        [(ax . >= . (* 0.5 +epsilon.0))
         (define y (+ 1.0 x))
         (- (fllog y) (/ (- (- y 1.0) x) y))]
        [else  x]))

(: log1p (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Single-Flonum -> Single-Flonum)
                 (Real -> Real)))
(define (log1p x)
  (cond [(double-flonum? x)  (fllog1p x)]
        [(single-flonum? x)  (real->single-flonum (fllog1p (real->double-flonum x)))]
        [(eqv? x 0)  0]
        [else  (fllog1p (real->double-flonum x))]))
