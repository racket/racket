#lang typed/racket/base

(require "../../flonum.rkt"
         "../polynomial/chebyshev.rkt")

(provide fllambert lambert fllambert- lambert-)

(define -lambert-max.0 (- (exp -1.0)))

(: lambert-upper-appx+ (Float -> Float))
(define (lambert-upper-appx+ x)
  (cond [(x . <= . 3)  (define z (+ x 1.04))
                       (define z^2 (* z z))
                       (define z^4 (* z^2 z^2))
                       (* 0.607 (flsqrt x) (- 1.0 (/ 1.0 z^4)))]
        [else  (define L1 (fllog x))
               (define L2 (fllog L1))
               (+ (- L1 L2) (/ L2 L1) (/ (* L2 (+ -2.0 L2))
                                         (* 2.0 L1 L1)))]))

(: lambert-upper-appx- (Float -> Float))
(define (lambert-upper-appx- x)
  (- (flexpt (+ 2.0 (/ (+ x -lambert-max.0)
                       (- -lambert-max.0)))
             0.4)
     1.0))

(: lambert-upper-newton (Float Float -> Float))
(define (lambert-upper-newton x y)
  (let loop ([y y] [n 0])
    (cond [(n . < . 6)
           (define exp-y (exp y))
           (define denom (* (+ y 1.0) exp-y))
           (define new-y (/ (+ (* y denom) (- x (* y exp-y)))
                            denom))
           (cond [((abs (- new-y y)) . <= . (abs (* +epsilon.0 new-y)))  y]
                 [else  (loop new-y (+ n 1))])]
          [else  y])))

(: fllambert (Float -> Float))
(define (fllambert x)
  (cond [(x . <= . -lambert-max.0)  (if (= x -lambert-max.0) -1.0 +nan.0)]
        [(x . > . 0.0)
         (cond [(x . > . 1e308)  (lambert-upper-appx+ x)]
               [else  (lambert-upper-newton x (lambert-upper-appx+ x))])]
        [(x . < . 0.0)  (lambert-upper-newton x (lambert-upper-appx- x))]
        [else  0.0]))

;; ===================================================================================================

(: lambert-lower-appx (Float -> Float))
(define (lambert-lower-appx x)
  (cond [(x . > . -0.3678793)
         (define L1 (fllog (- x)))
         (define L2 (fllog (- L1)))
         (+ (- L1 L2) (/ L2 L1) (/ (* L2 (+ -2.0 L2))
                                   (* 2.0 L1 L1)))]
        [(x . > . -0.3678794)
         ((inline-chebyshev-flpoly-fun
           -0.3678794 -0.367879
           (-2.002168474311089
            -0.0005183728424209184
            6.618450077030176e-05
            -1.718303045243156e-05
            5.513014540399948e-06
            -1.7451631748784575e-06))
          x)]
        [else
         ((inline-chebyshev-flpoly-fun
           -0.36787944 -0.3678794
           (-2.000625593512204
            -0.0001831934682653043
            2.9969169663135453e-05
            -1.002911105281908e-05
            4.0654407009362474e-06
            -1.5323076251738676e-06))
          x)]))

(: lambert-lower-newton (Float Float -> Float))
(define (lambert-lower-newton x y)
  (let loop ([dy 0.0] [y y] [n 0])
    (cond [(n . < . 13)
           (define exp-y (exp y))
           #;(define denom (* (+ y 1.0) exp-y))
           (define denom (- (exp (+ (fllog (abs (+ y 1.0))) y))))
           (define new-dy (/ (+ (- x denom) exp-y) denom))
           (define new-y (+ y new-dy))
           (cond [((abs (- new-y y)) . <= . (abs (* +epsilon.0 new-y)))  y]
                 [(and (n . > . 3) (not (= (flsgn new-dy) (flsgn dy))))
                  ;; If we detect oscillation, the true value is between new-y and y
                  (* 0.5 (+ new-y y))]
                 [else
                  (loop new-dy new-y (+ n 1))])]
          [else  y])))

(: fllambert- (Float -> Float))
(define (fllambert- x)
  (cond [(x . <= . -lambert-max.0)  (if (= x -lambert-max.0) -1.0 +nan.0)]
        [(x . >= . 0.0)  (if (= x 0.0) -inf.0 +nan.0)]
        [else  (lambert-lower-newton x (lambert-lower-appx x))]))
  
;; ===================================================================================================

(: lambert (case-> (Single-Flonum -> Single-Flonum)
                   (Flonum -> Flonum)
                   (Zero -> Zero)
                   (Real -> Real)))
(define (lambert x)
  (cond [(double-flonum? x)  (fllambert x)]
        [(single-flonum? x)  (real->single-flonum (fllambert (fl x)))]
        [(zero? x)  x]
        [else  (fllambert (fl x))]))

(: lambert- (case-> (Single-Flonum -> Single-Flonum)
                    (Flonum -> Flonum)
                    (Zero -> Zero)
                    (Real -> Real)))
(define (lambert- x)
  (cond [(double-flonum? x)  (fllambert- x)]
        [(single-flonum? x)  (real->single-flonum (fllambert- (fl x)))]
        [(zero? x)  x]
        [else  (fllambert- (fl x))]))
  