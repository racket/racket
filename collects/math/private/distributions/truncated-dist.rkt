#lang racket

#|
(module defs typed/racket
  
  (require racket/flonum
           math/distributions
           math/types)
  
  (provide (all-defined-out))
  
  (: truncated-pdf (Real-Density-Function Real-Distribution-Function Real Real
                                          -> Real-Density-Function))
  (define (truncated-pdf pdf cdf a b)
    (let ([a  (min a b)]
          [b  (max a b)])
      (define p (- (cdf b) (cdf a)))
      (: new-pdf Real-Density-Function)
      (define (new-pdf x [log? #f])
        (cond [log?  (- (pdf x #t) (fllog p))]
              [else  (/ (pdf x #f) p)]))
      new-pdf))
  )

(require 'defs plot math/distributions)

(plot (function (truncated-pdf (exp-pdf 1) (exp-cdf 1) 1 2) 0 3))
|#
