#lang typed/racket/base

(require "../../flonum.rkt")

(provide fllambert lambert)

(: lambert-appx+ (Float -> Float))
(define (lambert-appx+ x)
  (cond [(x . <= . 3)  (define z (+ x 1.04))
                       (define z^2 (* z z))
                       (define z^4 (* z^2 z^2))
                       (* 0.607 (flsqrt x) (- 1.0 (/ 1.0 z^4)))]
        [else  (define L1 (fllog x))
               (define L2 (fllog L1))
               (+ (- L1 L2) (/ L2 L1) (/ (* L2 (+ -2.0 L2))
                                         (* 2.0 L1 L1)))]))

(: lambert-appx- (Float -> Float))
(define (lambert-appx- x)
  (- (flexpt (+ 2.0 (/ (+ x -lambert-max.0)
                       (- -lambert-max.0)))
             0.4)
     1.0))

(: lambert-newton (Float Float -> Float))
(define (lambert-newton x y)
  (let loop ([y y] [n 0])
    (cond [(n . < . 6)
           (define exp-y (exp y))
           (define denom (* (+ y 1.0) exp-y))
           (define new-y (/ (+ (* y denom) (- x (* y exp-y)))
                            denom))
           (cond [((abs (- new-y y)) . <= . (abs (* 0.5 new-y +epsilon.0)))  y]
                 [else  (loop new-y (+ n 1))])]
          [else  y])))

(define -lambert-max.0 (- (exp -1.0)))

(: fllambert (Float -> Float))
(define (fllambert x)
  (cond [(x . <= . -lambert-max.0)  (if (= x -lambert-max.0) -1.0 +nan.0)]
        [(x . > . 0.0)
         (cond [(x . > . 1e308)  (lambert-appx+ x)]
               [else  (lambert-newton x (lambert-appx+ x))])]
        [(x . < . 0.0)  (lambert-newton x (lambert-appx- x))]
        [else  0.0]))

(: lambert (case-> (Single-Flonum -> Single-Flonum)
                   (Flonum -> Flonum)
                   (Zero -> Zero)
                   (Real -> Real)))
(define (lambert x)
  (cond [(double-flonum? x)  (fllambert x)]
        [(single-flonum? x)  (real->single-flonum (fllambert (fl x)))]
        [(zero? x)  x]
        [else  (fllambert (fl x))]))
