#lang typed/racket/base

#|
Arithmetic based on:

Jonathan Richard Shewchuk
Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
Discrete & Computational Geometry 18(3):305–363, October 1997
|#

(require "../flonum-functions.rkt"
         "../flonum-syntax.rkt"
         "../../bigfloat/bigfloat-struct.rkt")

(provide fl2 bigfloat->fl2 fl2->real fl2->bigfloat
         fl2+ fl2- fl2*split-fl fl2* fl2/
         flsqrt/error fl2sqrt)

;; ===================================================================================================
;; Conversion

(: fl2 (case-> (Real -> (Values Flonum Flonum))
               (Flonum Flonum -> (Values Flonum Flonum))))
(define fl2
  (case-lambda
    [(x)
     (cond [(flonum? x)  (values x 0.0)]
           [(single-flonum? x)  (values (fl x) 0.0)]
           [else  (let ([x2  (fl x)])
                    (values x2 (fl (- x (inexact->exact x2)))))])]
    [(x y)
     (fast-fl+/error x y)]))

(: bigfloat->fl2 (Bigfloat -> (Values Flonum Flonum)))
(define (bigfloat->fl2 x)
  (define x2 (bigfloat->flonum x))
  (values x2 (bigfloat->flonum (bf- x (bf x2)))))

(: fl2->real (Flonum Flonum -> Real))
(define (fl2->real x2 x1)
  (cond [(and (x1 . fl> . -inf.0) (x1 . fl< . +inf.0)
              (x2 . fl> . -inf.0) (x2 . fl< . +inf.0))
         (+ (inexact->exact x2) (inexact->exact x1))]
        [else  (fl+ x1 x2)]))

(: fl2->bigfloat (Flonum Flonum -> Bigfloat))
(define (fl2->bigfloat x2 x1)
  (bf+ (bf x1) (bf x2)))

(: fl3->fl2 (Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (fl3->fl2 e3 e2 e1)
  (values e3 (fl+ e2 e1)))

(: fl4->fl2 (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (fl4->fl2 e4 e3 e2 e1)
  (values e4 (fl+ e3 (fl+ e2 e1))))

;; ===================================================================================================
;; Addition and subtraction

(: raw-fl2+fl (Flonum Flonum Flonum -> (Values Flonum Flonum Flonum)))
(define (raw-fl2+fl e2 e1 b)
  (let*-values ([(Q h1)  (fast-fl+/error b e1)]
                [(h3 h2)  (fast-fl+/error Q e2)])
    (values h3 h2 h1)))

(: raw-fl2+ (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum Flonum Flonum)))
(define (raw-fl2+ e2 e1 f2 f1)
  (let*-values ([(h3 h2 h1)  (raw-fl2+fl e2 e1 f1)]
                [(h4 h3 h2)  (raw-fl2+fl h3 h2 f2)])
    (values h4 h3 h2 h1)))

(: fl2+ (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define fl2+
  (case-lambda
    [(e2 e1 b)
     (let-values ([(h3 h2 h1)  (raw-fl2+fl e2 e1 b)])
       (fl3->fl2 h3 h2 h1))]
    [(x2 x1 y2 y1)
     (let*-values ([(e4 e3 e2 e1)  (raw-fl2+ x2 x1 y2 y1)])
       (fl4->fl2 e4 e3 e2 e1))]))

(: fl2- (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define fl2-
  (case-lambda
    [(e2 e1 b)
     (let-values ([(h3 h2 h1)  (raw-fl2+fl e2 e1 (- b))])
       (fl3->fl2 h3 h2 h1))]
    [(x2 x1 y2 y1)
     (let*-values ([(e4 e3 e2 e1)  (raw-fl2+ x2 x1 (- y2) (- y1))])
       (fl4->fl2 e4 e3 e2 e1))]))

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
(define fl2*
  (case-lambda
    [(e2 e1 b)
     (let-values ([(b-hi b-lo)  (flsplit b)])
       (fl2*split-fl e2 e1 b-hi b-lo))]
    [(x2 x1 y2 y1)
     (let*-values ([(x2-hi x2-lo)  (flsplit x2)]
                   [(x1-hi x1-lo)  (flsplit x1)]
                   [(y2-hi y2-lo)  (flsplit y2)]
                   [(y1-hi y1-lo)  (flsplit y1)]
                   [(a2 a1)  (split-fl2*split-fl x2-hi x2-lo x1-hi x1-lo y1-hi y1-lo)]
                   [(b2 b1)  (split-fl2*split-fl x2-hi x2-lo x1-hi x1-lo y2-hi y2-lo)])
       (fl2+ a2 a1 b2 b1))]))

(: fl2/ (case-> (Flonum Flonum Flonum -> (Values Flonum Flonum))
                (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))))
(define fl2/
  (case-lambda
    [(x2 x1 y)
     (let*-values ([(a2 a1)  (fast-fl//error x1 y)]
                   [(b2 b1)  (fast-fl//error x2 y)])
       (fl2+ a2 a1 b2 b1))]
    [(x2 x1 y2 y1)
     ;; Compute three "digits" (flonums) of two-flonum long division; the third ensures the result is
     ;; correctly rounded
     (let*-values ([(z2)  (fl/ x2 y2)]
                   [(w2 w1)  (fl2* y2 y1 z2)]
                   [(x2 x1)  (fl2- x2 x1 w2 w1)]
                   [(z1)  (fl/ x2 y2)]
                   [(w2 w1)  (fl2* y2 y1 z1)]
                   [(x2 x1)  (fl2- x2 x1 w2 w1)])
       (fl3->fl2 z2 z1 (/ x2 y2)))]))

;; ===================================================================================================
;; Square roots

(: flsqrt/error (Flonum -> (Values Flonum Flonum)))
;; One-flonum estimate followed by one Newton's method iteration
;; This could be a little faster if `y' were split only once
(define (flsqrt/error x)
  (let*-values ([(y)  (flsqrt x)]
                [(z2 z1)  (fast-flsqr/error y)]
                [(dy2 dy1)  (fl2+ (- z2) (- z1) x)]
                [(dy2 dy1)  (fl2/ dy2 dy1 y)])
    (fl2+ (* 0.5 dy2) (* 0.5 dy1) y)))

(: fl2sqrt (Flonum Flonum -> (Values Flonum Flonum)))
(define (fl2sqrt x2 x1)
  (let*-values ([(y)  (flsqrt (fl+ x1 x2))]
                [(z2 z1)  (fast-flsqr/error y)]
                [(dy2 dy1)  (fl2- x2 x1 z2 z1)]
                [(dy2 dy1)  (fl2/ dy2 dy1 y)])
    (fl2+ (* 0.5 dy2) (* 0.5 dy1) y)))
