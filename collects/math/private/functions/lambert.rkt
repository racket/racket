#lang typed/racket/base

(require racket/fixnum
         "../../flonum.rkt"
         "../polynomial/chebyshev.rkt")

(provide fllambert lambert fllambert- lambert-)

(define -lambert-max.0 (- (flexp -1.0)))

(: lambert-upper-appx+ (Flonum -> Flonum))
(define (lambert-upper-appx+ x)
  (cond [(x . fl<= . 3.0)  (define z (fl+ x 1.04))
                           (define z^2 (fl* z z))
                           (fl* (fl* 0.607 (flsqrt x)) (fl- 1.0 (fl/ 1.0 (fl* z^2 z^2))))]
        [else  (define L1 (fllog x))
               (define L2 (fllog L1))
               (fl+ (fl+ (fl- L1 L2) (fl/ L2 L1))
                    (fl/ (fl* L2 (fl+ -2.0 L2))
                         (fl* (fl* 2.0 L1) L1)))]))

(: lambert-upper-appx- (Float -> Float))
(define (lambert-upper-appx- x)
  (fl- (flexpt (fl+ 2.0 (fl/ (fl+ x -lambert-max.0)
                             (- -lambert-max.0)))
             0.4)
       1.0))

(: lambert-upper-newton (Float Float -> Float))
(define (lambert-upper-newton x y)
  (let loop ([y y] [#{n : Nonnegative-Fixnum} 0])
    (cond [(n . fx< . 6)
           (define exp-y (flexp y))
           (define denom (fl* (fl+ y 1.0) exp-y))
           (define new-y (fl/ (fl+ (fl* y denom) (fl- x (fl* y exp-y)))
                              denom))
           (cond [((flabs (fl- new-y y)) . fl<= . (flabs (fl* epsilon.0 new-y)))  y]
                 [else  (loop new-y (fx+ n 1))])]
          [else  y])))

(: fllambert (Flonum -> Flonum))
(define (fllambert x)
  (cond [(x . fl> . 0.0)
         (cond [(x . fl> . 1e308)  (lambert-upper-appx+ x)]
               [else  (lambert-upper-newton x (lambert-upper-appx+ x))])]
        [(x . fl< . 0.0)
         (cond [(x . fl> . -lambert-max.0)  (lambert-upper-newton x (lambert-upper-appx- x))]
               [(x . fl< . -lambert-max.0)  +nan.0]
               [else  -1.0])]
        [else  x]))

;; ===================================================================================================

(: lambert-lower-appx (Float -> Float))
(define (lambert-lower-appx x)
  (cond [(x . fl> . -0.3678793)
         (define L1 (fllog (- x)))
         (define L2 (fllog (- L1)))
         (fl+ (fl+ (- L1 L2) (/ L2 L1))
              (fl/ (fl* L2 (fl+ -2.0 L2))
                   (fl* (fl* 2.0 L1) L1)))]
        [(x . fl> . -0.3678794)
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
  (let loop ([dy 0.0] [y y] [#{n : Nonnegative-Fixnum} 0])
    (cond [(n . fx< . 13)
           (define exp-y (flexp y))
           (define denom (- (flexp (fl+ (fllog (flabs (fl+ y 1.0))) y))))
           (define new-dy (fl/ (fl+ (fl- x denom) exp-y) denom))
           (define new-y (fl+ y new-dy))
           (cond [((flabs (fl- new-y y)) . fl<= . (flabs (fl* epsilon.0 new-y)))  y]
                 [(and (n . fx> . 3) (not (fl= (flsgn new-dy) (flsgn dy))))
                  ;; If we detect oscillation, the true value is between new-y and y
                  (fl* 0.5 (fl+ new-y y))]
                 [else
                  (loop new-dy new-y (fx+ n 1))])]
          [else  y])))

(: fllambert- (Float -> Float))
(define (fllambert- x)
  (cond [(x . fl<= . -lambert-max.0)  (if (fl= x -lambert-max.0) -1.0 +nan.0)]
        [(x . fl>= . 0.0)  (if (fl= x 0.0) -inf.0 +nan.0)]
        [else  (lambert-lower-newton x (lambert-lower-appx x))]))

;; ===================================================================================================

(: lambert (case-> (Zero -> Zero)
                   (Flonum -> Flonum)
                   (Real -> (U Zero Flonum))))
(define (lambert x)
  (cond [(flonum? x)  (fllambert x)]
        [(single-flonum? x)  (fllambert (fl x))]
        [(zero? x)  x]
        [(x . < . -lambert-max.0)
         (raise-argument-error 'lambert "Real >= (- (exp -1))" x)]
        [else  (fllambert (fl x))]))

(: lambert- (Real -> Flonum))
(define (lambert- x)
  (cond [(flonum? x)  (fllambert- x)]
        [(single-flonum? x)  (fllambert- (fl x))]
        [(or (x . < . -lambert-max.0) (x . >= . 0))
         (raise-argument-error 'lambert- "Negative-Real >= (- (exp -1))" x)]
        [else  (fllambert- (fl x))]))
