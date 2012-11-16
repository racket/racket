#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../distributions/impl/normal-cdf.rkt")

(provide flgamma-normal)

;; Temme's normal approximation for regularized incomplete gamma functions
;; This is much better than moment-matching or Wilson-Hilferty

(: flgamma-normal (Float Float Any Any -> Float))
(define (flgamma-normal k x log? upper?)
  (define l (fl/ x k))
  (define norm-x
    (cond [(or (l . fl< . epsilon.0) (l . fl> . (fl/ 1.0 epsilon.0)))
           ;; Avoid under-/overflow in calculating norm-x by doing it in log space
           (define log-l (fl- (fllog x) (fllog k)))
           (define l-1 (flexpm1 log-l))
           (define l-1-sign (flsgn l-1))
           (define log-n (fl* 0.5 (fl+ (fllog 2.0) (fllog (fl- l-1 log-l)))))
           (fl* l-1-sign (flexp (fl+ log-n (fl* 0.5 (fllog k)))))]
          [else
           (define n (fl* (flsgn (fl- l 1.0)) (flsqrt (fl* 2.0 (fl- (fl- l 1.0) (fllog l))))))
           (fl* n (flsqrt k))]))
  (let ([norm-x  (if upper? (- norm-x) norm-x)])
    (cond [log?  (standard-flnormal-log-cdf norm-x)]
          [else  (standard-flnormal-cdf norm-x)])))

