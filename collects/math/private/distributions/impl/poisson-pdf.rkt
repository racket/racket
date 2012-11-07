#lang typed/racket/base

;; These functions accept non-integral `k' because they're used for the gamma distribution as well
;; as the Poisson distribution

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt"
         "../../functions/stirling-error.rkt"
         "../normal-dist.rkt")

(provide flpoisson-pdf flpoisson-log-pdf)

(: flpoisson-pdf-limits (Flonum Flonum Any -> Flonum))
(define (flpoisson-pdf-limits l k log?)
  (cond [(l . fl<= . 0.0)
         (cond [(fl= l 0.0)  (cond [(fl= k 0.0)  (if log? 0.0 1.0)]
                                   [else  (if log? -inf.0 0.0)])]
               [else  +nan.0])]
        [(k . fl<= . 0.0)
         (cond [(fl= k 0.0)  (if log? (- l) (flexp (- l)))]
               [else  (if log? -inf.0 0.0)])]
        [else  +nan.0]))

(: flpoisson-in-bounds? (Flonum Flonum -> Boolean))
(define (flpoisson-in-bounds? l k)
  (and (l . fl> . 0.0) (k . fl> . 0.0) (l . fl< . +inf.0) (k . fl< . +inf.0)))

(define log-c (fllog (flsqrt +max-subnormal.0)))

(: get-div (Flonum Flonum -> Flonum))
;; Returns a divisor d that ensures l^(k/d)/k^(k/d)*exp((k-l)/d) won't underflow; assumes l and
;; k are positive
(define (get-div l k)
  (define d0 (fl/ (fl* k (fllog (fl/ l k))) log-c))
  (define d1 (fl/ (fl- k l) log-c))
  (define d (max d0 d1))
  (flmax 1.0 (flexpt 2.0 (flfloor (fl/ (fllog d) (fllog 2.0))))))

(: flpoisson-pdf (Flonum Flonum -> Flonum))
;; Maximum relative error depends on l and k:
;;   l,k < 4000    5 ulps
;;   l,k < 1e4    18 ulps
;;   l,k < 1e5    65 ulps
;;   l,k < 1e6   170 ulps
;;   l,k < 1e7   600 ulps
;; This is due to the argument reduction used in these areas to keep exponentials from underflowing
;; (search for `get-div' below)
(define (flpoisson-pdf l k)
  (cond [(not (flpoisson-in-bounds? l k))
         (flpoisson-pdf-limits l k #f)]
        ;; When l is large enough, there's no density > +min.0/2 within 48 stddevs
        [(and (l . fl> . 500.0) ((flabs (fl- k l)) . fl> . (fl* 48.0 (flsqrt l))))  0.0]
        ;; Use the direct definition when it won't over/underflow
        [(and (l . fl< . 700.0) (k . fl> . 1.0) (k . fl< . 100.0))
         (fl/ (fl/ (fl* (flexpt l k) (flexp (- l))) (flgamma k)) k)]
        [(k . fl<= . 1.0)
         ;; Using Stirling error works great here
         (define-values (l-k l-k-lo) (fast-fl-/error l k))
         (fl/ (* (flexp (- l-k))
                 (flexpt l k)
                 (flexpt k (- k))
                 (flexp (- l-k-lo))
                 (/ 1.0 (flsqrt (fl* 2.0 pi))))
              (fl* (flexp-stirling k) (flsqrt k)))]
        ;; When l > 1e21, the normal approximation has *lower* relative error than the algorithm
        ;; below. It *does not* have *low* relative error! It's about 2e9...
        ;; The normal approximation is perfect starting around 4.2e34, which is incidentally the
        ;; same time that 48 standard deviations from `l' becomes its floating-point neighbor.
        [(l . fl>= . 1e21)  (flnormal-pdf l (flsqrt l) k #f)]
        [else
         (define-values (l-k l-k-lo) (fast-fl-/error l k))
         (define-values (l/k l/k-lo) (fast-fl//error l k))
         (define div (get-div l k))
         (define y
           (fl* (flexpt (fl* (flexpt+ l/k l/k-lo (fl/ k div))
                             (flexp (fl/ (- l-k) div)))
                        div)
                (flexp (- l-k-lo))))
         (fl/ (fl* y (fl/ 1.0 (flsqrt (fl* 2.0 pi))))
              (fl* (flexp-stirling k) (flsqrt k)))]))

(: fllog-gamma1p-taylor-0 (Flonum -> Flonum))
;; Good for k < 0.005
(define fllog-gamma1p-taylor-0
  (make-flpolyfun
   (0.0
    -5.772156649015328606065120900824024310422e-1
    8.224670334241132182362075833230125946095e-1
    -4.00685634386531428466579387170483330255e-1
    2.705808084277845478790009241352919756937e-1
    -2.073855510286739852662730972914068336114e-1
    1.69557176997408189952419654965153421317e-1)))

(: flpoisson-log-pdf (Flonum Flonum -> Flonum))
(define (flpoisson-log-pdf l k)
  (cond [(not (flpoisson-in-bounds? l k))
         (flpoisson-pdf-limits l k #t)]
        [(k . fl<= . 0.005)
         ;; Don't subtract log(k)+log(gamma(k)) (which suffers from cancellation here), subtract
         ;; terms from its Taylor series
         (fl- (fl+ (fl* k (fllog l)) (- l))
              (fllog-gamma1p-taylor-0 k))]
        [(and (k . fl< . 1.0) (l . fl< . 1.0))
         (cond [(or (k . fl<= . 0.96) (l . fl> . 1e-300))
                (fl- (fllog (fl* (flexpt l k) (flexp (- l))))
                     (fl+ (fllog k) (fllog-gamma k)))]
               [else
                (- (fl* k (fllog l)) (fllog k) (fllog-gamma k) l)])]
        [(or (k . fl< . 2.0) (l . fl< . 2.0))
         (- (fl* k (fllog l)) (fllog k) (fllog-gamma k) l)]
        [(and (k . fl> . 2.0) (l . fl> . 2.0)
              ((fl- k l) . fl< . (fl* 40.0 (flsqrt l)))
              ((fl- l k) . fl< . (fl* 29.0 (flsqrt l))))
         ;; Error <= 1 ulp when flpoisson-pdf error is just a few ulps
         (fllog (flpoisson-pdf l k))]
        [(and (or (l . fl> . 1e18) (k . fl> . 1e18))
              (or ((fl- k l) . fl> . (fl* 0.5 k))
                  ((fl- l k) . fl> . (fl* 0.5 l))))
         ;; Asymptotic expansion had by replacing log(gamma(k)) with k*log(k)-k
         (- (fl* k (fl+ 1.0 (fllog (fl/ l k)))) (fllog k) l)]
        [else
         ;; Log version of the above; relative error here also climbs with distance from 0
         (define-values (l-k l-k-lo) (fast-fl-/error l k))
         (define-values (l/k l/k-lo) (fast-fl//error l k))
         (define div (get-div l k))
         (- (fl* div (fllog (fl* (flexpt+ l/k l/k-lo (fl/ k div))
                                 (flexp (fl/ (- l-k) div)))))
            (flstirling k)
            (fllog (* (flsqrt k) (flsqrt (fl* 2.0 pi)) (flexp l-k-lo))))]))
