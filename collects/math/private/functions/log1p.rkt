#lang typed/racket/base

(require racket/flonum
         "../../constants.rkt")

(provide fllog1p log1p)

;; Compute the value of log(1+x) in a way that is accurate for small x

(: fllog1p (case->
            ;(Float-Negative-Zero -> Float-Negative-Zero)
            ;(Float-Positive-Zero -> Float-Positive-Zero)
            (Float -> Float)))
(define (fllog1p x)
  (define ax (abs x))
  (cond [(ax . >= . 1.0)  (fllog (+ 1.0 x))]
        [(ax . >= . (* 0.5 +epsilon.0))
         (define y (+ 1.0 x))
         (- (fllog y) (/ (- (- y 1.0) x) y))]
        [else  x]))

(: log1p (case->
          ;(Zero -> Zero)
          ;(Float-Negative-Zero -> Float-Negative-Zero)
          ;(Float-Positive-Zero -> Float-Positive-Zero)
          ;(Float -> Float)
          (Real -> Real)))
(define (log1p x)
  (cond [(zero? x)  x]
        [(double-flonum? x)  (fllog1p x)]
        [(single-flonum? x)  (real->single-flonum (fllog1p (real->double-flonum x)))]
        [else  (fllog1p (real->double-flonum x))]))
