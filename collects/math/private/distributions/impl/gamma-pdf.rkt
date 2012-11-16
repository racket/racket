#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt"
         "../../functions/lambert.rkt"
         "poisson-pdf.rkt")

(provide standard-flgamma-log-pdf
         standard-flgamma-pdf)

(: standard-flgamma-pdf (Float Float -> Float))
;; Error <= 3 ulps for k < 1
(define (standard-flgamma-pdf k x)
  (cond [(k . fl< . 0.0)  +nan.0]
        [(k . fl>= . 1.0)  (flpoisson-pdf x (fl- k 1.0))]
        [(x . fl<= . 0.0)  (if (fl= x 0.0) +inf.0 0.0)]
        [(k . fl= . 0.0)  (if (fl= k 0.0) 0.0 +nan.0)]
        [(x . fl> . 750.0)  0.0]
        [(k . fl<= . 1e-16)
         ;; Asymptotic expansion had by replacing gamma(k) with 1/k
         ;; Switch on x to avoid underflow
         (cond [(x . fl> . 1e-16)
                (fl* (fl/ (fl* (flexpt x k) (flexp (- x))) x) k)]
               [else
                (fl/ (fl* (fl* (flexpt x k) (flexp (- x))) k) x)])]
        [else
         (cond [(or (x . fl> . 1e-300) (k . fl< . 0.1))
                (fl/ (fl/ (fl* (flexpt x k) (flexp (- x))) (flgamma k)) x)]
               [else
                ;; Deal with subnormal x by normalizing first
                (define d (flexpt 2.0 (flround (fl/ (fllog (flsqrt x)) (fllog 2.0)))))
                (fl* (fl/ (flexpt (fl/ x d) k) (flgamma k))
                     (fl/ (flexpt d k) x))])]))

(: flgamma-pdf-invert (Flonum Flonum -> Flonum))
(define (flgamma-pdf-invert k p)
  (fl* (fl- 1.0 k)
       (fllambert (fl/ 1.0 (fl* (flexpt (fl/ p (flgamma k)) (fl/ 1.0 (fl- k 1.0)))
                                (fl- 1.0 k))))))

(: standard-flgamma-log-pdf (Float Float -> Float))
(define (standard-flgamma-log-pdf k x)
  (cond [(k . fl< . 0.0)  +nan.0]
        [(k . fl>= . 1.0)  (flpoisson-log-pdf x (fl- k 1.0))]
        [(x . fl<= . 0.0)  (if (fl= x 0.0) +inf.0 -inf.0)]
        [(k . fl= . 0.0)  (if (fl= k 0.0) -inf.0 +nan.0)]
        ;; When k is small, log(gamma(k)) ~ -log(k), so -log(x)-log(gamma(k)) ~ -log(x/k)
        [(k . fl< . 1e-20)
         (define y (fllog1p (fl/ (fl- x k) k)))
         (cond [(rational? y)  (fl- (fl- (fl* k (fllog x)) x) y)]
               [else  (fl- (fl- (fl- (fl* k (fllog x)) (fllog x)) x) (fllog-gamma k))])]
        [(x . fl< . 0.4)
         (cond #;[((flabs (fl- x (flgamma-pdf-invert k 1.0))) . fl< . 0.05)
                  ;; This spot exhibits catastrophic cancellation; I can detect it, but I don't
                  ;; know what to do about it (besides compute very slowly in higher precision)
                  +nan.0]
               [(k . fl< . 0.5)
                (fl- (fl- (fl- (fl* k (fllog x)) (fllog x)) x)
                     (fllog-gamma k))]
               [else
                (fl- (fl- (fl* (fl- k 1.0) (fllog x)) x) (fllog-gamma k))])]
        [else
         (fl- (fl- (fl* (fl- k 1.0) (fllog x)) x) (fllog-gamma k))]))
