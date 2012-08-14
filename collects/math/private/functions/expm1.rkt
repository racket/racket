#lang typed/racket/base

(require racket/flonum
         "../../flonum.rkt"
         "polyfun.rkt")

(provide flexpm1 expm1)

;; Compute the value of exp(x)-1 in a way that is accurate for small x

(define expm1-poly-numer
  (make-polyfun
   Float
   (-0.28127670288085937e-1
     0.51278186299064534e0
    -0.6310029069350198e-1
     0.11638457975729296e-1
    -0.52143390687521003e-3
     0.21491399776965688e-4)))

(define expm1-poly-denom
  (make-polyfun
   Float
   ( 1.0
    -0.45442309511354755e0
     0.90850389570911714e-1
    -0.10088963629815502e-1
     0.63003407478692265e-3
    -0.17976570003654402e-4)))

(: flexpm1/poly (Float -> Float))
(define (flexpm1/poly x)
  ;; Define negative in terms of positive to avoid cancellation error
  (cond [(x . < . 0.0)  (define y (flexpm1/poly (- x)))
                        (/ (- y) (+ y 1.0))]
        [else  (+ (* x 0.10281276702880859e1)
                  (* x (/ (expm1-poly-numer x) (expm1-poly-denom x))))]))

(: flexpm1 (Float -> Float))
(define (flexpm1 x)
  (define ax (abs x))
  (cond [(ax . >= . 0.5)  (- (exp x) 1.0)]
        [(ax . > . (* 0.5 +epsilon.0))  (flexpm1/poly x)]
        [else  x]))

(: expm1 (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Single-Flonum -> Single-Flonum)
                 (Real -> Real)))
(define (expm1 x)
  (cond [(double-flonum? x)  (flexpm1 x)]
        [(single-flonum? x)  (real->single-flonum (flexpm1 (real->double-flonum x)))]
        [(eqv? x 0)  0]
        [else  (flexpm1 (real->double-flonum x))]))
