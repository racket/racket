#lang typed/racket/base

#|
Algorithms taken from:

N M Temme. A Set of Algorithms for the Incomplete Gamma Functions.
Probability in the Engineering and Informational Sciences, 1994, vol. 8, pp. 291--307.

For certain algorithms for a few subdomains of RxR, the above paper references this one:

W Gautschi. A Computational Procedure for Incomplete Gamma Functions.
ACM Transactions on Mathematical Software, 1979, vol. 5, pp. 466--481.

This implementation extends those in the papers in three ways:

 * Results are much more accurate, usually with relative error less than 100 ulps

 * Log-space results.
 
 * The functions return sensible rational answers on the largest domain possible.
|#

(require "../../flonum.rkt"
         "gamma.rkt"
         "log-gamma.rkt"
         "incomplete-gamma/gamma-lower-series.rkt"
         "incomplete-gamma/gamma-upper-frac.rkt"
         "incomplete-gamma/gamma-normal.rkt"
         "incomplete-gamma/gamma-gautschi.rkt"
         "incomplete-gamma/gamma-temme.rkt"
         "incomplete-gamma/gamma-utils.rkt")

(provide fllog-gamma-inc
         flgamma-inc
         log-gamma-inc
         gamma-inc)

(define: alg : (U 'normal 'temme 'gautschi 'series 'frac #f) #f)
(define (get-alg) alg)

(define temme-upper-thresh (make-parameter 2.0))
(define temme-lower-thresh (make-parameter 6.0))

(: use-log-normal? (Float Float Any -> Boolean))
;; Determines whether to use the normal cdf approximation
(define (use-log-normal? k x upper?)
  (and (k . fl> . 1e10)
       (or (if upper? (x . fl< . k) (x . fl> . k))
           (k . fl> . 1e32))))

(: use-temme? (Float Float -> Boolean))
;; Determines whether to use Temme's series; i.e. returns #t when k and x are close
(define (use-temme? k x)
  (and (k . fl> . 10.0)
       (or (and (x . fl>= . k)
                ((fl/ (flabs (fl- x k)) k) . fl< . (temme-upper-thresh)))
           (and (x . fl< . k)
                ((fl/ (flabs (fl- x k)) x) . fl< . (temme-lower-thresh))))))

;; ===================================================================================================
;; Argument reductions

(: flgamma-lower-reduction-sum (Float Float Float -> Float))
(define (flgamma-lower-reduction-sum k x n)
  (let loop ([y  0.0] [dy  1.0] [i  0.0])
    ;(printf "dy = ~v~n" dy) 
    (define new-y (fl+ y dy))
    (cond [(or (i . >= . n)
               (not (rational? new-y))
               ((flabs dy) . fl<= . (flabs (fl* epsilon.0 new-y))))
           y]
          [else
           (loop new-y (fl* dy (fl/ x (fl+ k (fl+ 1.0 i)))) (+ i 1.0))])))

(: flgamma-upper-reduction-sum (Float Float Float -> Float))
(define (flgamma-upper-reduction-sum k x n)
  (let loop ([y  0.0] [dy  1.0] [i  0.0])
    ;(printf "dy = ~v~n" dy) 
    (define new-y (fl+ y dy))
    (cond [(or (i . >= . n)
               (not (rational? new-y))
               ((flabs dy) . fl<= . (flabs (fl* epsilon.0 new-y))))
           y]
          [else
           (loop new-y (fl* dy (fl/ (fl- k (fl+ 1.0 i)) x)) (+ i 1.0))])))

;; ===================================================================================================
;; Regularized incomplete gamma functions

(: flgamma-in-bounds? (Flonum Flonum -> Boolean))
(define (flgamma-in-bounds? k x)
  (and (k . fl> . 0.0) (k . fl< . +inf.0) (x . fl> . 0.0) (x . fl< . +inf.0)))

(: flgamma-lower-limits (Flonum Flonum -> Flonum))
(define (flgamma-lower-limits k x)
  (cond [(or (k . fl< . 0.0) (x . fl< . 0.0))  +nan.0]
        ;; k = +inf.0: a step function with the step "at infinity"
        [(fl= k +inf.0)  (if (fl= x +inf.0) 1.0 0.0)]
        ;; k = 0.0: a step function with the step at 0.0
        [(fl= k 0.0)  1.0]
        [(fl= x 0.0)  0.0]
        [(fl= x +inf.0)  1.0]
        ;; k is +nan.0 and x is +nan.0
        [else  +nan.0]))

(: flgamma-lower-regularized* (Flonum Flonum -> Flonum))
(define (flgamma-lower-regularized* k x)
  (cond [(k . fl< . 1e-20)  1.0]
        [(or (k . < . 150.0) (x . < . 150.0))
         ;; When k and x are small enough, use reliable and accurate series and continued fraction
         ;; Could do this on a larger domain, but the number of iterations for both would be very
         ;; large when k ~ x
         (cond [(or (x . < . 2.0)
                    (and (k . < . 4.0) (k . >= . (- x (flsqrt k))))
                    (and (k . >= . 4.0) (k . >= . x)))
                (set! alg 'series)
                (flgamma-lower-series k x)]
               [else
                (set! alg 'frac)
                (- 1.0 (flgamma-upper-frac k x))])]
        [(k . fl> . (* 1.75 x))
         (set! alg 'series)
         (flgamma-lower-series k x)]
        [(and (k . fl< . 1e17) ((- x k) . > . (* 5.0 (flsqrt k))))
         (set! alg 'frac)
         (- 1.0 (flgamma-upper-frac k x))]
        [(or (and (k . < . x) (k . fl> . 1e26))
             (and (k . >= . x) (k . fl> . 1e29)))
         (set! alg 'normal)
         (flgamma-normal k x #f #f)]
        [else
         (set! alg 'temme)
         (flgamma-temme k x #f)]))

(: flgamma-lower-regularized (Float Float -> Float))
(define (flgamma-lower-regularized k x)
  (set! alg #f)
  (cond [(not (flgamma-in-bounds? k x))  (flgamma-lower-limits k x)]
        [(and (k . > . 15.0)
              (k . > . x)
              (k . <= . (* 1.75 x)))
         ;; Argument "reduction"; cap at 100 extra iterations `n' because it would be effectively
         ;; unbounded otherwise (we lose accuracy for bounding this)
         ;; For some reason, even when k+n,x is in a bad spot, this still helps a lot
         (define n (flmin 100.0 (flfloor (flsqrt (max 0.0 (fl- (fl* 2.0 x) k))))))
         (fl+ (flgamma-lower-regularized* (fl+ k n) x)
              (fl* (flgamma-series-const k x)
                   (flgamma-lower-reduction-sum k x n)))]
        [else
         (flgamma-lower-regularized* k x)]))

(: flgamma-upper-regularized* (Flonum Flonum -> Flonum))
(define (flgamma-upper-regularized* k x)
  (cond [(and (k . < . 1.0) (x . < . 1.75))
         (set! alg 'gautschi)
         (flgamma-upper-gautschi k x #f)]
        [(or (k . < . 150.0) (x . < . 150.0))
         (cond [(k . >= . x)
                (set! alg 'series)
                (- 1.0 (flgamma-lower-series k x))]
               [else
                (set! alg 'frac)
                (flgamma-upper-frac k x)])]
        [(k . fl> . (* 1.75 x))
         (set! alg 'series)
         (fl- 1.0 (flgamma-lower-series k x))]
        [(and (k . fl< . 1e17) ((- x k) . > . (* 2.0 (flsqrt k))))
         (set! alg 'frac)
         (flgamma-upper-frac k x)]
        [(or (and (k . < . x) (k . fl> . 1e29))
             (and (k . >= . x) (k . fl> . 1e26)))
         (set! alg 'normal)
         (flgamma-normal k x #f #t)]
        [else
         (set! alg 'temme)
         (flgamma-temme k x #t)]))

(: flgamma-upper-regularized (Float Float -> Float))
(define (flgamma-upper-regularized k x)
  (set! alg #f)
  (cond [(not (flgamma-in-bounds? k x))  (- 1.0 (flgamma-lower-limits k x))]
        [(and (k . > . 15.0)
              (k . < . x)
              (k . > . (* 0.5 x)))
         (define n (flmin 100.0 (flfloor (flsqrt (max 0.0 (fl- (fl* 2.0 k) x))))))
         (fl+ (flgamma-upper-regularized* (fl- k n) x)
              (fl* (/ (flgamma-upper-const k x) x)
                   (flgamma-upper-reduction-sum k x n)))]
        [else
         (flgamma-upper-regularized* k x)]))

(: fllog-gamma-lower-regularized (Float Float -> Float))
(define (fllog-gamma-lower-regularized k x)
  (set! alg #f)
  (cond [(not (flgamma-in-bounds? k x))  (fllog (flgamma-lower-limits k x))]
        [(and (k . < . 1.0) (x . < . 1.75))
         (set! alg 'gautschi)
         (flgamma-lower-gautschi k x #t)]
        [(and (k . >= . x) (> (fl+ (fl* k (fl- (fllog x) (fllog k))) (fl- k x))
                              (fllog +max-subnormal.0)))
         (fllog (flgamma-lower-regularized k x))]
        [(and (k . < . x) (> (fl+ (fl* k (fl- (fllog x) (fllog k))) (fl- k x))
                             (fllog +max-subnormal.0)))
         (fllog1p (- (flgamma-upper-regularized k x)))]
        [(k . fl> . (* 1.75 x))
         (set! alg 'series)
         (fllog-gamma-lower-series k x)]
        [(and (k . fl< . 1e17) (k . < . x))
         (set! alg 'frac)
         (fllog1p (- (flgamma-upper-frac k x)))]
        [(use-log-normal? k x #f)
         (set! alg 'normal)
         (flgamma-normal k x #t #f)]
        [else
         (set! alg 'temme)
         (fllog-gamma-temme k x #f)]))

(: fllog-gamma-upper-regularized (Float Float -> Float))
(define (fllog-gamma-upper-regularized k x)
  (set! alg #f)
  (cond [(not (flgamma-in-bounds? k x))  (fllog (- 1.0 (flgamma-lower-limits k x)))]
        [(or (and (k . fl>= . 1.0) ((fl/ x k) . fl> . 1e19))
             (and (k . fl< . 1.0) (x . fl> . 1e19)))
         (- x)]
        [(and (k . < . 1.0) (x . < . 1.75))
         (set! alg 'gautschi)
         (flgamma-upper-gautschi k x #t)]
        [(and (k . < . x) (> (fl+ (fl* k (fl- (fllog x) (fllog k))) (fl- k x))
                             (fllog +max-subnormal.0)))
         (fllog (flgamma-upper-regularized k x))]
        [(and (k . >= . x) (> (fl+ (fl* k (fl- (fllog x) (fllog k))) (fl- k x))
                              (fllog +max-subnormal.0)))
         (fllog1p (- (flgamma-lower-regularized k x)))]
        [(k . fl> . (* 1.75 x))
         (set! alg 'series)
         (fllog1p (- (flgamma-lower-series k x)))]
        [(and (k . fl< . 1e17) (k . < . x))
         (set! alg 'frac)
         (fllog-gamma-upper-frac k x)]
        [(use-log-normal? k x #t)
         (set! alg 'normal)
         (flgamma-normal k x #t #t)]
        [else
         (set! alg 'temme)
         (fllog-gamma-temme k x #t)]))

;; ===================================================================================================
;; User-facing gamma functions

(: fllog-gamma-inc (Float Float Any Any -> Float))
(define (fllog-gamma-inc k x upper? regularized?)
  (define z
    (cond [upper?  (fllog-gamma-upper-regularized k x)]
          [else    (fllog-gamma-lower-regularized k x)]))
  (cond [regularized?  z]
        [else  (fl+ z (fllog-gamma k))]))

(: flgamma-inc (Float Float Any Any -> Float))
(define (flgamma-inc k x upper? regularized?)
  (define z
    (cond [upper?  (flgamma-upper-regularized k x)]
          [else    (flgamma-lower-regularized k x)]))
  (cond [regularized?  z]
        [else  (fl* z (flgamma k))]))

(define-syntax-rule (define-incomplete-gamma-wrapper name flname)
  (begin
    (: name (case-> (Real Real -> Float)
                    (Real Real Any -> Float)
                    (Real Real Any Any -> Float)))
    (define (name k x [upper? #f] [regularized? #f])
      (cond [(and (exact? k) (k . <= . 0))
             (raise-argument-error 'name "Positive-Real" 0 k x)]
            [(and (exact? x) (x . < . 0))
             (raise-argument-error 'name "Nonnegative-Real" 1 k x)]
            [else  (flname (fl k) (fl x) upper? regularized?)]))))

(define-incomplete-gamma-wrapper gamma-inc flgamma-inc)
(define-incomplete-gamma-wrapper log-gamma-inc fllog-gamma-inc)
