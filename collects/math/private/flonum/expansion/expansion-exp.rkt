#lang typed/racket/base

#|
Compute exp and expm1 with 105-bit accuracy

In case anybody cares, the argument reduction scheme used here is about a 3x improvement in speed
over current state-of-the-art for computing exponentials using a Taylor series. The main discovery
that enables it is that it's perfectly okay-and-accuracy-preserving to subtract powers of a base
other than 2, such as 1.25. See "expansion-exp-reduction.rkt" for details.

I may turn this into a paper someday...

Note to self: exp(x+y)-1 = (exp(x)-1) * (exp(y)-1) + (exp(x)-1) + (exp(y)-1)
|#

(require racket/fixnum
         "../../../base.rkt"
         "../../../bigfloat.rkt"
         "../flonum-functions.rkt"
         "../flonum-error.rkt"
         "../flonum-exp.rkt"
         "../flonum-constants.rkt"
         "expansion-base.rkt"
         "expansion-exp-reduction.rkt")

(provide flexp/error fl2exp
         flexpm1/error fl2expm1)

;; ===================================================================================================
;; Helper functions

(define-values (c3-hi c3-lo) (fl2 1/6))
(define-values (c4-hi c4-lo) (fl2 1/24))
(define-values (c5-hi c5-lo) (fl2 1/120))
(define-values (c6-hi c6-lo) (fl2 1/720))
(define-values (c7-hi c7-lo) (fl2 1/5040))
(define-values (c8-hi c8-lo) (fl2 1/40320))

(: flexpm1-small/error (Flonum -> (Values Flonum Flonum)))
;; Computes exp(x)-1 when x is small, using 8 terms from its Taylor series using Horner's method
;; Relative error is <= 1e-32 when (abs x) < 0.0005
;; Relative error for (+ 1 (flexpm1-small/error x)) is <= 1e-32 when (abs x) < 0.00118
(define (flexpm1-small/error x)
  (let*-values ([(x-hi x-lo)  (flsplit x)]
                [(y2 y1)  (fl2*split-fl c8-hi c8-lo x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 c7-hi c7-lo)]
                [(y2 y1)  (fl2*split-fl y2 y1 x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 c6-hi c6-lo)]
                [(y2 y1)  (fl2*split-fl y2 y1 x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 c5-hi c5-lo)]
                [(y2 y1)  (fl2*split-fl y2 y1 x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 c4-hi c4-lo)]
                [(y2 y1)  (fl2*split-fl y2 y1 x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 c3-hi c3-lo)]
                [(y2 y1)  (fl2*split-fl y2 y1 x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 0.5)]
                [(y2 y1)  (fl2*split-fl y2 y1 x-hi x-lo)]
                [(y2 y1)  (fl2+ y2 y1 1.0)])
    (fl2*split-fl y2 y1 x-hi x-lo)))

(: flexpm1-tiny/error (Flonum -> (Values Flonum Flonum)))
;; Computes exp(x)-1 when x is friggin' tiny, like 1e-18 or something, using 2 terms from its Taylor
;; series at 0
;; I haven't bothered quantifying the error because this is only used for the low-order flonum in
;; fl2exp and fl2expm1, and those are accurate
(define (flexpm1-tiny/error x)
  (let-values ([(y2 y1)  (fast-fl+/error (* 0.5 x) 1.0)])
    (fl2* y2 y1 x)))

;; ===================================================================================================
;; exp

;; See "expansion-exp-reduction.rkt" for details on the argument reduction here

(: flexp/error (Flonum -> (Values Flonum Flonum)))
(define (flexp/error x)
  (let: loop : (Values Flonum Flonum) ([x : Flonum  x] [n : Nonnegative-Fixnum  0])
    (cond
      [(or ((flabs x) . fl< . exp-min) (n . fx> . 15))  ; even n > 5 should never happen
       (let-values ([(y2 y1)  (flexpm1-small/error x)])
         (fl2+ y2 y1 1.0))]
      [(x . fl< . (- (fllog +max.0)))  (values (flexp x) 0.0)]
      [(x . fl> . (fllog +max.0))  (values +inf.0 0.0)]
      [(rational? x)
       (let*-values ([(x d2 d1)  (flexpm1-reduction x)]
                     [(d2 d1)  (fl2+ d2 d1 1.0)]
                     [(y2 y1)  (loop x (fx+ n 1))])
         (fl2* d2 d1 y2 y1))]
      [else
       (values +nan.0 +nan.0)])))

(: fl2exp (Flonum Flonum -> (Values Flonum Flonum)))
(define (fl2exp x2 x1)
  (let*-values ([(a2 a1)  (flexp/error x2)]
                [(b2 b1)  (flexpm1-tiny/error x1)]
                [(b2 b1)  (fl2+ b2 b1 1.0)])
    (fl2* a2 a1 b2 b1)))

;; ===================================================================================================
;; expm1

#|
Argument reduction for expm1

Let `y' be chosen using exp's argument reduction, D = exp(y)-1, and R = exp(x-y)-1. Then

  exp(x)-1 = D * R + D + R

using the identity noted at the beginning of this file. Calculating this straightforwardly suffers
from severe cancellation (generally invalidating all but two bits in the low-order flonum), but
calculating this doesn't:

  exp(x)-1 = D * (R + 1) + R

The computation of R+1 definitely loses precision, but it doesn't seem to matter.
|#

(: flexpm1/error (Flonum -> (Values Flonum Flonum)))
(define (flexpm1/error x)
  (cond
    [(x . < . -1.0)
     ;; exp(x) is near zero here, so this is more accurate
     (let-values ([(y2 y1)  (flexp/error x)])
       (fl2+ y2 y1 -1.0))]
    [else
     (let: loop : (Values Flonum Flonum) ([x : Flonum  x] [n : Nonnegative-Fixnum  0])
       (cond [(or ((flabs x) . fl< . expm1-min) (n . fx> . 15))  ; even n > 5 should never happen
              (flexpm1-small/error x)]
             [(x . fl< . (- (fllog +max.0)))  (values -1.0 0.0)]
             [(x . fl> . (fllog +max.0))  (values +inf.0 0.0)]
             [(rational? x)
              (let*-values ([(x d2 d1)  (flexpm1-reduction x)]
                            [(r2 r1)  (loop x (fx+ n 1))]
                            [(w2 w1)  (fl2+ r2 r1 1.0)]
                            [(y2 y1)  (fl2* d2 d1 w2 w1)])
                (fl2+ y2 y1 r2 r1))]
             [else
              (values +nan.0 +nan.0)]))]))

(: fl2expm1 (Flonum Flonum -> (Values Flonum Flonum)))
(define (fl2expm1 x2 x1)
  (cond
    [((fl+ x1 x2) . < . -1.0)
     (let-values ([(y2 y1)  (fl2exp x2 x1)])
       (fl2+ y2 y1 -1.0))]
    [else
     (let*-values ([(a2 a1)  (flexpm1/error x2)]
                   [(b2 b1)  (flexpm1-tiny/error x1)]
                   [(w2 w1)  (fl2+ a2 a1 1.0)]
                   [(y2 y1)  (fl2* b2 b1 w2 w1)])
       (fl2+ y2 y1 a2 a1))]))
