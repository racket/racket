#lang typed/racket/base

;; Computes sums without incurring rounding error more than once
;; See (flsum '(1.0 1e-16 1e-16)) vs. (+ 1.0 1e-16 1e-16)

#|
Algorithm adapted from:

J R Shewchuk. Adaptive Precision Floating-Point Arithmetic and Fast Geometric Predicates.
Discrete & Computational Geometry, 1996, vol 18, pp 305--363.
|#

(require "../unsafe.rkt"
         "flonum-functions.rkt")

(provide flsum)

(: flsum ((Listof Flonum) -> Flonum))
(define (flsum xs)
  (define ys (make-flvector (length xs)))
  (define num-ys
    (for/fold: ([num-ys : Nonnegative-Fixnum  0]) ([x  (in-list xs)])
      (define-values (new-x i)
        (for/fold: ([x : Flonum  x] [i : Nonnegative-Fixnum  0]) ([p  (in-range num-ys)])
          (define y (unsafe-flvector-ref ys p))
          (let-values ([(x y)  (if ((flabs x) . fl< . (flabs y)) (values y x) (values x y))])
            (define z (fl+ x y))
            (define-values (hi lo)
              (cond [(rational? z)  (values z (fl- y (fl- z x)))]
                    [else  (values x y)]))
            (cond [(fl= lo 0.0)  (values hi i)]
                  [else  (unsafe-flvector-set! ys i lo)
                         (values hi (unsafe-fx+ i 1))]))))
      (unsafe-flvector-set! ys i new-x)
      (unsafe-fx+ i 1)))
  
  (for/fold: ([sum : Flonum  0.0]) ([p  (in-range num-ys)])
    (fl+ sum (unsafe-flvector-ref ys p))))
