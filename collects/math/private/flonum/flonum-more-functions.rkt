#lang typed/racket/base

(require "flonum-functions.rkt"
         "flonum-exp.rkt"
         "flonum-log.rkt")

(provide flsqrt1pm1)

(: flsqrt1pm1 (Float -> Float))
(define (flsqrt1pm1 x)
  (cond [((flabs x) . fl> . 0.75)
         (fl- (flsqrt (fl+ 1.0 x)) 1.0)]
        [else
         (flexpm1 (fl* 0.5 (fllog1p x)))]))

