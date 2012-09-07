#lang typed/racket/base

(require racket/performance-hint
         "../../../flonum.rkt"
         "normal-utils.rkt")

(provide standard-flnormal-log-pdf
         standard-flnormal-pdf)

(begin-encourage-inline
  
  (: standard-flnormal-log-pdf (Float -> Float))
  ;; For x small, relative error is less than +epsilon.0; stays under (* 2 +epsilon.0)
  (define (standard-flnormal-log-pdf x)
    (- (* -0.5 x x) logsqrt2pi.0))
  
  (: standard-flnormal-pdf (Float -> Float))
  (define (standard-flnormal-pdf x)
    (cond [(x . > . 37.8)
           ;; We're in the badlands... do *stuff* to reduce relative error
           (cond [(x . > . +standard-normal-pdf-max.0)  0.0]
                 [(x . > . 38.555)  +min.0]
                 [else
                  ;; Using the log pdf seems to be more stable here
                  (exp (standard-flnormal-log-pdf x))])]
          [else
           (/ (flexp-1/2*x^2 x) sqrt2pi.0)]))
  
  )  ; begin-encourage-inline
