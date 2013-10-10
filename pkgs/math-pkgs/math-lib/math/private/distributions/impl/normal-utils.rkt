#lang typed/racket/base

(require racket/performance-hint
         "../../../flonum.rkt")

(provide (all-defined-out))

;; More correct than what the FPU comes up with:
(define sqrt2pi.0 2.5066282746310007)
(define 1/sqrt2pi.0 0.3989422804014327)
(define logsqrt2pi.0 0.9189385332046728)
(define -logsqrt2pi.0 (- logsqrt2pi.0))

;; The largest input that returns a nonzero density:
(define +standard-normal-pdf-max.0 38.58015760902841)
;; The largest input that returns a nonzero log density:
(define +standard-normal-log-pdf-max.0 1.8961503816218352e+154)

;; The smallest input that returns a nonzero probability:
(define -standard-normal-cdf-max.0 -38.485408335567335)
;; The largest input that returns a non-one probability:
(define +standard-normal-cdf-max.0 8.292361075813595)

;; The smallest input that returns a finite log probability:
(define -standard-normal-log-cdf-max.0 -1.8961503816218352e+154)
;; The largest input that returns a nonzero log probability:
(define +standard-normal-log-cdf-max.0 38.48540833595427)

(begin-encourage-inline
  
  (: flexp-1/2*x^2 (Float -> Float))
  (define (flexp-1/2*x^2 x)
    (define trunc-x (fl/ (fltruncate (fl* (flexpt 2.0 21.0) x)) (flexpt 2.0 21.0)))
    (fl* (flexp (fl* (fl* -0.5 trunc-x) trunc-x))
         (flexp (fl* (fl* -0.5 (fl- x trunc-x)) (fl+ x trunc-x)))))
  
  )  ; begin-encourage-inline
