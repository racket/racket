#lang typed/racket/base

;; Calculate sqrt(1+x)-1 in a way that avoids underflow for small x

(require racket/flonum
         "log1p.rkt"
         "expm1.rkt")

(provide flsqrt1pm1 sqrt1pm1)

(: flsqrt1pm1 (Float -> Float))
(define (flsqrt1pm1 x)
  (cond [((abs x) . > . 0.75)  (- (flsqrt (+ 1.0 x)) 1.0)]
        [else  (flexpm1 (* 0.5 (fllog1p x)))]))

(: sqrt1pm1 (Real -> Real))
(define (sqrt1pm1 x)
  (cond [(zero? x)  x]
        [(double-flonum? x)  (flsqrt1pm1 x)]
        [(single-flonum? x)  (real->single-flonum (flsqrt1pm1 (real->double-flonum x)))]
        [else  (flsqrt1pm1 (real->double-flonum x))]))
