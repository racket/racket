#lang typed/racket/base

(require racket/performance-hint
         "../../../flonum.rkt"
         "normal-utils.rkt")

(provide standard-flnormal-log-pdf
         standard-flnormal-pdf)

(begin-encourage-inline
  
  (: standard-flnormal-log-pdf (Float -> Float))
  ;; For x small, relative error is less than epsilon.0; stays under (* 2 epsilon.0)
  (define (standard-flnormal-log-pdf x)
    (fl- (fl* (fl* -0.5 x) x) logsqrt2pi.0))
  
  (: standard-flnormal-pdf (Float -> Float))
  (define (standard-flnormal-pdf x)
    (cond [(x . fl> . 37.8)
           ;; We're in the badlands... do *stuff* to reduce relative error
           (cond [(x . fl> . +standard-normal-pdf-max.0)  0.0]
                 [(x . fl> . 38.555)  +min.0]
                 [else
                  ;; Using the log pdf seems to be more stable here
                  (flexp (standard-flnormal-log-pdf x))])]
          [else
           (fl/ (flexp-1/2*x^2 x) sqrt2pi.0)]))
  
  )  ; begin-encourage-inline
