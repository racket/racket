#lang typed/racket/base

(require "../../flonum.rkt")

(provide fldelta-pdf
         fldelta-cdf
         fldelta-inv-cdf
         fldelta-random)

(: fldelta-pdf (Float Float Any -> Float))
(define (fldelta-pdf x0 x log?)
  (cond [(= x x0)  +inf.0]
        [else  (if log? -inf.0 0.0)]))

(: fldelta-cdf (Float Float Any Any -> Float))
(define (fldelta-cdf x0 x log? upper-tail?)
  (cond [(x . < . x0)
         (cond [upper-tail?  (if log? 0.0 1.0)]
               [else  (if log? -inf.0 0.0)])]
        [else
         (cond [upper-tail?  (if log? -inf.0 0.0)]
               [else  (if log? 0.0 1.0)])]))

(: fldelta-inv-cdf (Float Float Any Any -> Float))
(define (fldelta-inv-cdf x0 q log? upper-tail?)
  (cond [(not (flprobability? q log?))  +nan.0]
        [upper-tail?
         (cond [log?  (if (q . > . -inf.0) x0 +inf.0)]
               [else  (if (q . > . 0.0) x0 +inf.0)])]
        [else
         (cond [log?  (if (q . < . 0.0) x0 +inf.0)]
               [else  (if (q . < . 1.0) x0 +inf.0)])]))

(: fldelta-random (Float -> Float))
(define (fldelta-random x0) x0)
