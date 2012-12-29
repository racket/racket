#lang typed/racket/base

#|
Arithmetic based on:

Jonathan Richard Shewchuk
Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
Discrete & Computational Geometry 18(3):305–363, October 1997

Other parts shamelessly stolen from crlibm (which is LGPL)
|#

(require "../flonum-functions.rkt"
         "../flonum-bits.rkt"
         "../flonum-error.rkt"
         "../flonum-constants.rkt"
         "../utils.rkt")

(provide fl2? fl2 fl2->real
         fl2ulp fl2ulp-error
         +max-fl2-subnormal.0 -max-fl2-subnormal.0
         fl2abs fl2+ fl2- fl2*split-fl fl2* fl2sqr fl2/
         fl2sqrt flsqrt/error)

(: floverlapping? (Flonum Flonum -> Boolean))
(define (floverlapping? x2 x1)
  (define-values (s2 e2) (flonum->sig+exp (flabs x2)))
  (define-values (s1 e1) (flonum->sig+exp (flabs x1)))
  (define-values (n1 n2)
    (if (e2 . > . e1)
        (values s1 (arithmetic-shift s2 (- e2 e1)))
        (values (arithmetic-shift s1 (- e1 e2)) s2)))
  (not (= (bitwise-ior n1 n2)
          (bitwise-xor n1 n2))))

(: fl2? (Flonum Flonum -> Boolean))
(define (fl2? x2 x1)
  (cond [(flrational? x2)
         (cond [(flrational? x1)
                (cond [((flabs x2) . < . (flabs x1))  #f]
                      [else  (not (floverlapping? x2 x1))])]
               [else  #f])]
        [else  (fl= x1 0.0)]))

;; ===================================================================================================
;; Conversion

(: fl2 (Real -> (Values Flonum Flonum)))
(define (fl2 x)
  (cond [(flonum? x)  (values x 0.0)]
        [(single-flonum? x)  (values (fl x) 0.0)]
        [else  (let ([x2  (fl x)])
                 (cond [(flrational? x2)
                        (let ([x2  (fl+ x2 (fl (- x (inexact->exact x2))))])
                          (cond [(flrational? x2)
                                 (values x2 (fl (- x (inexact->exact x2))))]
                                [else  (values x2 0.0)]))]
                       [else  (values x2 0.0)]))]))

(: fl2->real (Flonum Flonum -> Real))
(define (fl2->real x2 x1)
  (cond [(and (flrational? x2) (flrational? x1))
         (+ (inexact->exact x2) (inexact->exact x1))]
        [else  (fl+ x1 x2)]))

(: fl4->fl2 (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (fl4->fl2 e4 e3 e2 e1)
  (values e4 (fl+ e3 (fl+ e2 e1))))

;; ===================================================================================================
;; Error

(: fl2ulp (Flonum Flonum -> Flonum))
(define (fl2ulp x2 x1)
  (cond [(fl= x2 0.0)  0.0]
        [else  (flmax +min.0 (fl* (flulp x2) (flexpt 2.0 -52.0)))]))

(: fl2ulp-error (Flonum Flonum Real -> Flonum))
(define (fl2ulp-error x2 x1 r)
  (define x (fl2->real x2 x1))
  (define-values (r2 r1) (fl2 r))
  (cond [(eqv? x r)  0.0]
        [(and (fl= x2 0.0) (fl= r2 0.0))  0.0]
        [(and (fl= x2 +inf.0) (fl= r2 +inf.0))  0.0]
        [(and (fl= x2 -inf.0) (fl= r2 -inf.0))  0.0]
        [(zero? r)  +inf.0]
        [(and (rational? x) (flrational? r2))
         (flabs (fl (/ (- (inexact->exact x) (inexact->exact r))
                       (inexact->exact (flmax +min.0 (fl2ulp r2 r1))))))]
        [else  +inf.0]))

(define +max-fl2-subnormal.0 1.0020841800044863e-292)  ; (flprev (flexpt 2.0 -970.0))
(define -max-fl2-subnormal.0 (- +max-fl2-subnormal.0))

;; ===================================================================================================
;; Absolute value

(: fl2abs (case-> (Flonum -> (Values Flonum Flonum))
                  (Flonum Flonum -> (Values Flonum Flonum))))
(define (fl2abs x2 [x1 0.0])
  (define x (fl+ x2 x1))
  (cond [(not (flrational? x))  (values (flabs x2) 0.0)]
        [(fl= x 0.0)  (values 0.0 0.0)]
        [(fl> x 0.0)  (values x2 x1)]
        [else  (values (- x2) (- x1))]))

;; ===================================================================================================
;; Addition and subtraction

(: fl2+ (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define (fl2+ x2 x1 y2 [y1 0.0])
  (define r (fl+ x2 y2))
  (cond [(not (flrational? r))  (values r 0.0)]
        [(and (fl= x2 0.0) (fl= y2 0.0))  (values r 0.0)]
        [else
         (define s (if ((flabs x2) . fl> . (flabs y2))
                       (fl+ (fl+ (fl+ (fl- x2 r) y2) y1) x1)
                       (fl+ (fl+ (fl+ (fl- y2 r) x2) x1) y1)))
         (define z2 (fl+ r s))
         (values z2 (fl+ (fl- r z2) s))]))

(: fl2- (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define (fl2- x2 x1 y2 [y1 0.0])
  (fl2+ x2 x1 (- y2) (- y1)))

;; ===================================================================================================
;; Multiplication and division

(: raw-split-fl2*split-fl (Flonum Flonum Flonum Flonum Flonum Flonum
                                  -> (Values Flonum Flonum Flonum Flonum)))
(define (raw-split-fl2*split-fl e2-hi e2-lo e1-hi e1-lo b-hi b-lo)
  (let*-values ([(b)   (fl+ b-lo b-hi)]
                [(Q1)  (fl* (fl+ e1-lo e1-hi) b)]
                [(h1)  (- (- Q1
                             (fl* e1-hi b-hi)
                             (fl* e1-lo b-hi)
                             (fl* e1-hi b-lo)
                             (fl* e1-lo b-lo)))]
                [(T)  (fl* (fl+ e2-lo e2-hi) b)]
                [(t)  (- (- T
                            (fl* e2-hi b-hi)
                            (fl* e2-lo b-hi)
                            (fl* e2-hi b-lo)
                            (fl* e2-lo b-lo)))]
                [(Q2 h2)  (fast-fl+/error Q1 t)]
                [(h4 h3)  (fast-mono-fl+/error T Q2)])
    (values h4 h3 h2 h1)))

(: split-fl2*split-fl (Flonum Flonum Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (split-fl2*split-fl e2-hi e2-lo e1-hi e1-lo b-hi b-lo)
  (let-values ([(h4 h3 h2 h1)  (raw-split-fl2*split-fl e2-hi e2-lo e1-hi e1-lo b-hi b-lo)])
    (fl4->fl2 h4 h3 h2 h1)))

(: fl2*split-fl (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (fl2*split-fl e2 e1 b-hi b-lo)
  (let*-values ([(e2-hi e2-lo)  (flsplit e2)]
                [(e1-hi e1-lo)  (flsplit e1)]
                [(h4 h3 h2 h1)  (raw-split-fl2*split-fl e2-hi e2-lo e1-hi e1-lo b-hi b-lo)])
    (fl4->fl2 h4 h3 h2 h1)))

(: fl2* (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define (fl2* x2 x1 y2 [y1 0.0])
  (define z (fl* x2 y2))
  (cond [(and (flrational? z) (not (fl= z 0.0)) (not (flsubnormal? z)))
         (define dx (near-pow2 x2))
         (define dy (near-pow2 y2))
         (define d (fl* dx dy))
         (let* ([x2  (fl/ x2 dx)]
                [x1  (fl/ x1 dx)]
                [y2  (fl/ y2 dy)]
                [y1  (fl/ y1 dy)]
                [up  (fl* x2 (fl+ 1.0 (flexpt 2.0 27.0)))]
                [vp  (fl* y2 (fl+ 1.0 (flexpt 2.0 27.0)))]
                [u1  (fl+ (fl- x2 up) up)]
                [v1  (fl+ (fl- y2 vp) vp)]
                [u2  (fl- x2 u1)]
                [v2  (fl- y2 v1)]
                [m2  (fl* x2 y2)]
                [m1  (fl+ (fl+ (fl+ (fl+ (fl+ (fl- (fl* u1 v1) m2)
                                              (fl* u1 v2))
                                         (fl* u2 v1))
                                    (fl* u2 v2))
                               (fl* x2 y1))
                          (fl* x1 y2))]
                [z2  (fl+ m2 m1)]
                [z1  (fl+ (fl- m2 z2) m1)]
                [z2  (fl* d z2)])
           (values z2 (if (flrational? z2) (fl* d z1) 0.0)))]
        [else
         (values z 0.0)]))

(: fl2sqr (case-> (Flonum -> (Values Flonum Flonum))
                  (Flonum Flonum -> (Values Flonum Flonum))))
;; Derived from fl2*
(define fl2sqr
  (case-lambda
    [(x)  (flsqr/error x)]
    [(x2 x1)
     (define z (fl* x2 x2))
     (cond [(and (flrational? z) (not (fl= z 0.0)) (not (flsubnormal? z)))
            (define d (near-pow2 x2))
            (define d^2 (fl* d d))
            (let* ([x2  (fl/ x2 d)]
                   [x1  (fl/ x1 d)]
                   [up  (fl* x2 (fl+ 1.0 (flexpt 2.0 27.0)))]
                   [u1  (fl+ (fl- x2 up) up)]
                   [u2  (fl- x2 u1)]
                   [m2  (fl* x2 x2)]
                   [m1  (fl+ (fl+ (fl+ (fl- (fl* u1 u1) m2)
                                       (fl* 2.0 (fl* u1 u2)))
                                  (fl* u2 u2))
                             (fl* 2.0 (fl* x2 x1)))]
                   [z2  (fl+ m2 m1)]
                   [z1  (fl+ (fl- m2 z2) m1)]
                   [z2  (fl* d^2 z2)])
              (values z2 (if (flrational? z2) (fl* d^2 z1) 0.0)))]
           [else
            (values z 0.0)])]))

(: fl2/ (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define (fl2/ x2 x1 y2 [y1 0.0])
  (define z (fl/ x2 y2))
  (cond [(and (flrational? z) (not (fl= z 0.0)) (flrational? y2))
         (define d (near-pow2/div x2 y2))
         (let*-values ([(x2 x1)  (values (fl/ x2 d) (fl/ x1 d))]
                       [(y2 y1)  (values (fl/ y2 d) (fl/ y1 d))]
                       [(c2)  (fl/ x2 y2)]
                       [(u2 u1)  (fl*/error c2 y2)]
                       [(c1)  (fl/ (fl- (fl+ (fl- (fl- x2 u2) u1) x1) (fl* c2 y1)) y2)]
                       [(z2)  (fl+ c2 c1)])
           (values z2 (if (flrational? z2) (fl+ (fl- c2 z2) c1) 0.0)))]
        [else
         (values z 0.0)]))

;; ===================================================================================================
;; Square roots

(: fl2sqrt (case-> (Flonum -> (Values Flonum Flonum))
                   (Flonum Flonum -> (Values Flonum Flonum))))
;; One-flonum estimate followed by one Newton's method iteration
(define (fl2sqrt x2 [x1 0.0])
  (cond [(and (flrational? x2) (not (fl= x2 0.0)))
         (define-values (d^2 d)
           (cond [(x2 . fl<= . +max-fl2-subnormal.0)  (values (flexpt 2.0 -104.0)
                                                              (flexpt 2.0 -52.0))]
                 [(x2 . fl> . 1e300)  (values (flexpt 2.0 104.0)
                                              (flexpt 2.0 52.0))]
                 [else  (values 1.0 1.0)]))
         (let*-values ([(x2 x1)  (values (fl/ x2 d^2) (fl/ x1 d^2))]
                       [(y)  (flsqrt (fl+ x2 x1))]
                       [(z2 z1)  (fast-flsqr/error y)]
                       [(dy2 dy1)  (fl2- x2 x1 z2 z1)]
                       [(dy2 dy1)  (fl2/ dy2 dy1 y)]
                       [(y2 y1)  (fl2+ (fl* 0.5 dy2) (fl* 0.5 dy1) y)]
                       [(y2)  (fl* y2 d)])
           (values y2 (if (flrational? y2) (fl* y1 d) 0.0)))]
        [else
         (values (flsqrt x2) 0.0)]))

(: flsqrt/error (Flonum -> (Values Flonum Flonum)))
(define (flsqrt/error x) (fl2sqrt x 0.0))
