#lang typed/racket/base

(require racket/flonum
         "../constants.rkt"
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

(: flexpm1 (case->
            ;(Zero -> Zero)
            ;(Float-Negative-Zero -> Float-Negative-Zero)
            ;(Float-Positive-Zero -> Float-Positive-Zero)
            (Float -> Float)))
(define (flexpm1 x)
  (define ax (abs x))
  (cond [(ax . >= . 0.5)  (- (exp x) 1.0)]
        [(ax . > . (* 0.5 +epsilon.0))  (flexpm1/poly x)]
        [else  x]))

(: expm1 (case->
          ;(Zero -> Zero)
          ;(Float-Negative-Zero -> Float-Negative-Zero)
          ;(Float-Positive-Zero -> Float-Positive-Zero)
          ;(Float -> Float)
          (Real -> Real)))
(define (expm1 x)
  (cond [(zero? x)  x]
        [(double-flonum? x)  (flexpm1 x)]
        [(single-flonum? x)  (real->single-flonum (flexpm1 (real->double-flonum x)))]
        [else  (flexpm1 (real->double-flonum x))]))

(module* test typed/racket
  (require (submod "..") typed/rackunit math/constants)
  (define ε (* 2 +epsilon.0))
  
  (check-= (flexpm1 1.)      1.71828182845904523536028747135 ε)
  (check-= (flexpm1 1e-5)    0.000010000050000166667083 ε)
  (check-= (flexpm1 1e-10)   1.00000000005000000000166666667e-10 ε)
  (check-= (flexpm1 1e-15)   1.00000000000000050000000000000e-15 ε)
  (check-= (flexpm1 1e-15)   1.00000000000000050000000000000e-15 ε)
  (check-= (flexpm1 -1.)    -0.632120558828557678404476229839 ε)
  (check-= (flexpm1 -1e-10) -9.99999999950000000001666666667e-11 ε)
  (check-= (flexpm1 0.0)     0.0 ε)
  
  (check-equal? (expm1 0) 0)
  (check-= (expm1 1.0)   1.71828182845904523536028747135 ε)
  (check-= (expm1 1.0f0) 1.71828182845904523536028747135f0 ε)
  (check-= (expm1 1/2)   0.648721270700128146848650787814 ε))
