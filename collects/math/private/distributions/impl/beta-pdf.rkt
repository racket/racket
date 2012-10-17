#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../functions/beta.rkt")

(provide flbeta-log-pdf)

(: flbeta-log-pdf (Flonum Flonum Flonum -> Flonum))
(define (flbeta-log-pdf a b x)
  (cond [(or (a . fl<= . 0.0) (b . fl<= . 0.0))
         (cond [(or (a . fl< . 0.0) (b . fl< . 0.0))
                +nan.0]
               [(and (fl= a 0.0) (fl= b 0.0))
                (if (or (fl= x 0.0) (fl= x 1.0)) +inf.0 -inf.0)]
               [(fl= a 0.0)
                (if (fl= x 0.0) +inf.0 0.0)]
               [else
                (if (fl= x 1.0) +inf.0 0.0)])]
        [(or (x . fl< . 0.0) (x . fl> . 1.0))
         -inf.0]
        [else
         (flsum (list (fl* (fl- a 1.0) (fllog x))
                      (fl* (fl- b 1.0) (fllog1p (- x)))
                      (fl- 0.0 (fllog-beta a b))))]))
