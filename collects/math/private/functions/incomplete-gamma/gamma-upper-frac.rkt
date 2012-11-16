#lang typed/racket/base

;; Computes Legendre's continued fraction for the upper gamma function

(require "../../../flonum.rkt"
         "../log-gamma.rkt"
         "../continued-fraction.rkt"
         "gamma-utils.rkt")

(provide flgamma-upper-frac fllog-gamma-upper-frac get-frac-iters)

(define: frac-iters : Flonum  0.0)
(define (get-frac-iters) frac-iters)

(: flgamma-upper-iter (Float Float -> Float))
(define (flgamma-upper-iter k x)
  (continued-fraction 1.0
                      (λ (i a) (set! frac-iters i) (fl* i (fl- k i)))
                      (fl- (fl+ x 1.0) k)
                      (λ (i b) (fl+ b 2.0))
                      epsilon.0))

(: flgamma-upper-frac (Float Float -> Float))
(define (flgamma-upper-frac k x)
  (set! frac-iters 0.0)
  (define z (flgamma-upper-const k x))
  (cond [(fl= z 0.0)  0.0]
        [else  (fl* z (flgamma-upper-iter k x))]))

(: fllog-gamma-upper-frac (Float Float -> Float))
(define (fllog-gamma-upper-frac k x)
  (define y (flgamma-upper-iter k x))
  (define log-z (fllog-gamma-upper-const k x))
  (fl+ log-z (fllog y)))
