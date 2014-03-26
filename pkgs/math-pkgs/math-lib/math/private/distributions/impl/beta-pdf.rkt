#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../functions/beta.rkt")

(provide flbeta-log-pdf)

(: flbeta-log-pdf (Flonum Flonum Flonum -> Flonum))
(define (flbeta-log-pdf a b x)
  (cond [(or (a . fl< . 0.0) (b . fl< . 0.0)
             (and (fl= a 0.0) (fl= b 0.0))
             (and (fl= a +inf.0) (fl= b +inf.0)))
         +nan.0]
        [(or (fl= a 0.0) (fl= b +inf.0))
         (if (fl= x 0.0) +inf.0 -inf.0)]
        [(or (fl= b 0.0) (fl= a +inf.0))
         (if (fl= x 1.0) +inf.0 -inf.0)]
        [(or (x . fl< . 0.0) (x . fl> . 1.0))
         -inf.0]
        ;; Avoid (* 0.0 -inf.0) by taking a limit from the right
        [(and (fl= a 1.0) (fl= x 0.0))
         (- (fllog-beta 1.0 b))]
        ;; Avoid (* 0.0 -inf.0) by taking a limit from the left
        [(and (fl= b 1.0) (fl= x 1.0))
         (- (fllog-beta a 1.0))]
        [else
         (flsum (list (fl* (fl- a 1.0) (fllog x))
                      (fl* (fl- b 1.0) (fllog1p (- x)))
                      (- (fllog-beta a b))))]))
