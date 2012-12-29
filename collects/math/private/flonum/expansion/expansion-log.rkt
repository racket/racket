#lang typed/racket/base

#|
Compute log and log1p with 105-bit accuracy

Both implementations do some argument reduction, find excellent 53-bit initial estimates, and then
perform one Newton step.
|#

(require "../flonum-functions.rkt"
         "../flonum-error.rkt"
         "../flonum-log.rkt"
         "expansion-base.rkt"
         "expansion-exp.rkt")

(provide fl2log fl2log1p)

;; ===================================================================================================
;; log

#|
Argument reduction for log:

  log(x) = log(x*2^k) - k*log(2)

A value of k that reduces any x to [0.5, 1.0] is
  
  k = -truncate(log(x)/log(2))
|#

(define-values (log2-hi log2-lo) (values 0.6931471805599453 2.3190468138462996e-17))

(: fl2log-reduction (Flonum Flonum -> (Values Flonum Flonum Flonum)))
(define (fl2log-reduction x2 x1)
  (define k (- (fltruncate (fl/ (fllog+ x1 x2) (fllog 2.0)))))
  (cond [(k . fl> . 1023.0)
         ;; This can happen if x is subnormal; just multiply in pieces
         (define k0 1023.0)
         (define k1 (fl- k k0))
         (define 2^k0 (flexpt 2.0 k0))
         (define 2^k1 (flexpt 2.0 k1))
         (let*-values ([(x2 x1)  (values (* x2 2^k0 2^k1) (* x1 2^k0 2^k1))])
           (values k x2 x1))]
        [else
         (define 2^k (flexpt 2.0 k))
         (let*-values ([(x2 x1)  (values (fl* x2 2^k) (fl* x1 2^k))])
           (values k x2 x1))]))

(: fl2log (Flonum Flonum -> (Values Flonum Flonum)))
(define (fl2log x2 x1)
  (define x (fl+ x1 x2))
  (cond [(x . fl<= . 0.0)  (cond [(fl= x 0.0)  (values -inf.0 0.0)]
                                 [else  (values +nan.0 +nan.0)])]
        [(or (x . fl< . 0.5) (x . fl> . 2.5))
         ;; Reduce arguments
         (let*-values ([(k x2 x1)  (fl2log-reduction x2 x1)]
                       [(y2 y1)  (fl2log x2 x1)]
                       [(z2 z1)  (fl2* log2-hi log2-lo k)])
           (fl2- y2 y1 z2 z1))]
        [else
         ;; Estimate log(x) and do a Newton iteration using expm1
         (let*-values ([(y)  (fllog+ x2 x1)]
                       [(x2 x1)  (fl2+ x2 x1 -1.0)]
                       [(z2 z1)  (flexpm1/error y)]
                       [(w2 w1)  (fl2+ z2 z1 1.0)]
                       [(dy2 dy1)  (fl2- x2 x1 z2 z1)]
                       [(dy2 dy1)  (fl2/ dy2 dy1 w2 w1)])
           (fl2+ dy2 dy1 y))]))

;; ===================================================================================================
;; log1p

#|
Argument reduction for log1p:

  log1p(x) = k*log(2) + log1p(x/2^k + (1/2^k - 1))

A `k'  that reduces any argument `x' to (-1/2,1/2) is

  k = round(log1p(x)/log(2))
|#

(: fl2log1p-reduction (Flonum Flonum -> (Values Flonum Flonum Flonum)))
(define (fl2log1p-reduction x2 x1)
  (define-values (a2 a1) (fl2+ x2 x1 1.0))
  (define y (fllog+ a2 a1))
  (define k (flround (fl/ y (fllog 2.0))))
  (define 2^k (flexpt 2.0 k))
  (define-values (j2 j1) (fast-fl-/error (/ 1.0 2^k) 1.0))
  (let*-values ([(x2 x1)  (values (/ x2 2^k) (/ x1 2^k))]
                [(x2 x1)  (fl2+ x2 x1 j2 j1)])
    (values k x2 x1)))

(: fl2log1p (Flonum Flonum -> (Values Flonum Flonum)))
(define (fl2log1p x2 x1)
  (define-values (a2 a1) (fl2+ x2 x1 1.0))
  (define y (fllog+ a2 a1))
  (cond
    [(y . fl< . -0.5)  (fl2log a2 a1)]
    [(y . fl> . 0.5)
     (let*-values ([(k x2 x1)  (fl2log1p-reduction x2 x1)]
                   [(y2 y1)  (fl2log1p x2 x1)]
                   [(z2 z1)  (fl2* log2-hi log2-lo k)])
       (fl2+ y2 y1 z2 z1))]
    [else
     (let*-values ([(z2 z1)  (flexpm1/error y)]
                   [(w2 w1)  (fl2+ z2 z1 1.0)]
                   [(dy2 dy1)  (fl2- x2 x1 z2 z1)]
                   [(dy2 dy1)  (fl2/ dy2 dy1 w2 w1)])
       (fl2+ dy2 dy1 y))]))
