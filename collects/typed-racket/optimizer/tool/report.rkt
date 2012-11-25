#lang racket/base

(require "instrumentation.rkt" "profiling.rkt"
         "typed-racket.rkt" "inlining.rkt" "hidden-costs.rkt"
         "locality-merging.rkt")

(provide generate-report locality-merging)

;; profile is currently only used to refine the inlining logs
(define (generate-report this profile)
  (define-values (TR-log mzc-log info-log) (generate-logs this))
  (define hot-functions (and profile (prune-profile profile)))
  (append
   (report-typed-racket TR-log profile hot-functions)
   (if profile
       ;; inlining and hidden cost reports have too low a SNR to be shown
       ;; w/o profiling-based pruning
       (append (report-inlining mzc-log profile hot-functions)
               (report-hidden-costs info-log profile hot-functions))
       '())))
