#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "../../unsafe.rkt"
         "../../distributions/impl/normal-cdf.rkt"
         "../stirling-error.rkt"
         "gamma-utils.rkt")

(provide flgamma-temme fllog-gamma-temme)

;; ===================================================================================================
;; Temme's series for the incomplete gamma functions (used when k ~ x and k is not small)

(define temme-iters (make-parameter 32))

(define num-fs 100)
(define fs
  (let ()
    (define: start-fs : (Vectorof Real)  (vector 1 -1/3 1/12 -2/135))
    (define: fs : (Vectorof Real)  (make-vector num-fs 0))
    (vector-copy! fs 0 start-fs)
    ;; DP algorithm to compute f coefficients
    (for ([m  (in-range 4 num-fs)])
      (vector-set!
       fs m
       (* (- (/ (+ m 1) (+ m 2)))
          (+ (* (/ (- m 1) (* 3 m)) (vector-ref fs (- m 1)))
             (for/fold: ([sum : Real  0]) ([j  (in-range 3 m)])
               (+ sum (/ (* (vector-ref fs (- j 1)) (vector-ref fs (+ m 1 (- j))))
                         (+ m 2 (- j)))))))))
    (vector->flvector fs)))

(: R-sum (Float Flonum -> Flonum))
(define (R-sum k n)
  (define num-fs (temme-iters))
  ;; This originally filled a vector of bs, because imperative programmers don't know how to do
  ;; anything else besides bang an array full of values (sheesh)
  (define-values (sum b2 b1)
    (for/fold: ([sum : Flonum  0.0]
                [b2 : Flonum  (unsafe-flvector-ref fs (- num-fs 1))]
                [b1 : Flonum  (unsafe-flvector-ref fs (- num-fs 2))]
                ) ([m  (in-range (- num-fs 3) 0 -1)])
      (define c (unsafe-flvector-ref fs m))
      (define b0 (fl+ c (fl/ (fl* (fl+ (fl m) 1.0) b2) k)))
      (values (fl+ (fl* n sum) b0) b1 b0)))
  sum)

(: R-log (Float Float -> (Values Float Float)))
;; Log-space version of `R' above
(define (R-log k n)
  (define sum (R-sum k n))
  (values
   (fl- (fl- (fl- (fl+ (fllog (abs sum)) (fl* (fl* (fl* -0.5 k) n) n))
                  (fl* 0.5 (fllog k)))
             (fl* 0.5 (fllog (fl* 2.0 pi))))
        (flstirling k))
   (flsgn sum)))

(: flgamma-temme (Float Float Any -> Float))
;; Computes a regularized incomplete gamma using Temme's series
(define (flgamma-temme k x upper?)
  (define n
    (let ([l  (fl/ x k)])
      (fl* (flsgn (fl- l 1.0)) (flsqrt (fl* 2.0 (fl- (fl- l 1.0) (fllog l)))))))
  (define z (let ([z  (fl* n (flsqrt k))])
              (if upper? (- z) z)))
  (define r (let ([r  (fl* (R-sum k n) (flgamma-series-const k x))])
              (if upper? (- r) r)))
  (cond [(z . fl<= . 0.0)  (fl- (standard-flnormal-cdf z) r)]
        [else  (fl- 1.0 (fl+ (standard-flnormal-cdf (- z)) r))]))

(: fllog-gamma-temme (Float Float Any -> Float))
(define (fllog-gamma-temme k x upper?)
  (define n (let ([l  (fl/ x k)])
              (fl* (flsgn (fl- l 1.0)) (flsqrt (fl* 2.0 (fl- (fl- l 1.0) (fllog l)))))))
  (define z (let ([z  (fl* n (flsqrt k))])
              (if upper? (- z) z)))
  (define-values (log-r r-sgn) (let-values ([(log-r r-sgn)  (R-log k n)])
                                 (if upper? (values log-r (- r-sgn)) (values log-r r-sgn))))
  (cond [(z . fl<= . 0.0)
         (define norm-log-p (standard-flnormal-log-cdf z))
         (define log-p (if (r-sgn . fl< . 0.0)
                           (lg+ norm-log-p log-r)
                           (lg- norm-log-p log-r)))
         ;; When norm-log-p ~ log-r, the above log-space arithmetic can go bad
         ;; Fortunately, this means we don't need any correctional terms - a normal approximation is
         ;; good enough
         (if (rational? log-p) log-p norm-log-p)]
        [else
         (define norm-log-p (standard-flnormal-log-cdf (- z)))
         (define log-p (if (r-sgn . fl< . 0.0)
                           (lg1- (lg- norm-log-p log-r))
                           (lg1- (lg+ norm-log-p log-r))))
         (if (rational? log-p) log-p (lg1- norm-log-p))]))
