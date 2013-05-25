#lang typed/racket/base

(require "../../../flonum.rkt")

(provide fldelta-pdf
         fldelta-cdf
         fldelta-inv-cdf)

(: fldelta-pdf (Float Float Any -> Float))
(define (fldelta-pdf c x log?)
  (cond [(fl= x c)  +inf.0]
        [else  (if log? -inf.0 0.0)]))

(: fldelta-cdf (Float Float Any Any -> Float))
(define (fldelta-cdf c x log? 1-p?)
  (cond [(x . fl< . c)
         (cond [1-p?  (if log? 0.0 1.0)]
               [else  (if log? -inf.0 0.0)])]
        [else
         (cond [1-p?  (if log? -inf.0 0.0)]
               [else  (if log? 0.0 1.0)])]))

(: fldelta-inv-cdf (Float Float Any Any -> Float))
(define (fldelta-inv-cdf c q log? 1-p?)
  (cond [(not (flprobability? q log?))  +nan.0]
        [1-p?  (cond [log?  (if (q . fl> . -inf.0) c +inf.0)]
                     [else  (if (q . fl> . 0.0) c +inf.0)])]
        [else  (cond [log?  (if (q . fl< . 0.0) c +inf.0)]
                     [else  (if (q . fl< . 1.0) c +inf.0)])]))
