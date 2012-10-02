#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt")

(provide standard-flgamma-log-pdf
         standard-flgamma-pdf)

(: use-log? (Float Float -> Boolean))
;; Returns #t when (k,x) is in a region in which non-log intermediate results are too large
(define (use-log? k x)
  (or (k . fl< . 1e-20)
      (k . fl> . 170.0)
      ((fl+ x k) . fl> . 200.0)))

(: standard-flgamma-log-pdf (Float Float -> Float))
(define (standard-flgamma-log-pdf k x)
  (cond
    ;; When k is small, log(gamma(k)) ~ -log(k), so -log(x)-log(gamma(k)) ~ -log(x/k)
    [(k . fl< . 1e-20)
     (define y (fllog1p (fl/ (fl- x k) k)))
     (cond [(rational? y)  (fl- (fl- (fl* k (fllog x)) x) y)]
           [else  (fl- (fl- (fl- (fl* k (fllog x)) (fllog x)) x) (fllog-gamma k))])]
    ;; Subtracting 1.0 when k < 0.5 can lose a lot of precision
    [(k . fl< . 0.5)
     (fl- (fl- (fl- (fl* k (fllog x)) (fllog x)) x) (fllog-gamma k))]
    [else
     ;; Relative error grows without bound as x and k get large, but there's not a lot we can do
     ;; about that without a new approach
     (fl- (fl- (fl* (fl- k 1.0) (fllog x)) x) (fllog-gamma k))]))

(: standard-flgamma-pdf* (Float Float -> Float))
;; Assumes (use-log? k x) = #f
(define (standard-flgamma-pdf* k x)
  (cond
    ;; Subtracting 1.0 when k < 0.5 can lose a lot of precision
    [(k . fl< . 0.5)
     (fl/ (fl* (fl/ (flexpt x k) x) (flexp (- x))) (flgamma k))]
    [else
     (fl/ (fl* (flexpt x (fl- k 1.0)) (flexp (- x))) (flgamma k))]))

(: standard-flgamma-pdf (Float Float -> Float))
(define (standard-flgamma-pdf k x)
  (cond [(use-log? k x)  (exp (standard-flgamma-log-pdf k x))]
        [else  (standard-flgamma-pdf* k x)]))
