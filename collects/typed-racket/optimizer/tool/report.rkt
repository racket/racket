#lang racket/base

(require racket/class racket/match
         "structs.rkt" "instrumentation.rkt" "inlining.rkt" "hidden-costs.rkt"
         "locality-merging.rkt" "causality-merging.rkt")

(provide generate-report locality-merging)

;; profile is currently only used to refine the inlining logs
(define (generate-report this profile)
  (define-values (TR-log mzc-log info-log) (generate-logs this))
  (define hot-functions (and profile (prune-profile profile)))
  (append
   (log->report
    (append (causality-merging
             (prune-cold-TR-failures TR-log profile hot-functions))
            (if profile
                (report-hidden-costs info-log profile hot-functions)
                '())))
   (report-inlining mzc-log profile hot-functions)))


;; Returns a report-entry or #f, which means prune.
(define (log-entry->report-entry l)
  (match l
    [(log-entry kind msg stx located-stx (? number? pos) provenance)
     (define start     (sub1 pos))
     (define end       (+ start (syntax-span stx)))
     ;; When we first create report entries, they have a single sub.
     (report-entry (list (if (opt-log-entry? l)
                             (opt-report-entry located-stx msg provenance)
                             (missed-opt-report-entry
                              located-stx msg provenance
                              (missed-opt-log-entry-badness   l)
                              (missed-opt-log-entry-irritants l))))
                   start end
                   (if (opt-log-entry? l) ; badness
                       0
                       (missed-opt-log-entry-badness l)))]
    [_ #f])) ; no source location, ignore

;; converts log-entry structs to report-entry structs for further
;; processing
(define (log->report log)
  (filter values (map log-entry->report-entry log)))

;;--------------------------------------------------------------------

(require "profiling.rkt")
(define (prune-cold-TR-failures TR-log profile hot-functions)
  (define total-time (and profile (profile-total-time profile)))

  ;; #f if no profiling info is available for this function
  ;; takes in either a single pos number or a pair of numbers (line col)
  (define (pos->node pos)
    (and profile
         pos
         (for/first ([p (in-list (profile-nodes profile))]
                     #:when (let* ([from (node-pos p)]
                                   [span (node-span p)])
                              (and from span
                                   (<= from pos (+ from span)))))
           p)))

  (if (not profile)
      TR-log ; keep everything if we don't have profile info
      (for/list ([l (in-list TR-log)]
                 #:when (or (opt-log-entry? l) ; don't prune successes
                            ;; in hot function?
                            (memq (pos->node (log-entry-pos l)) hot-functions)))
        (define profile-entry (memq (pos->node (log-entry-pos l)) hot-functions))
        (define badness-multiplier
          (if profile-entry
              (/ (node-self (car profile-entry)) total-time)
              1))
        (match l
          [(missed-opt-log-entry kind msg stx located-stx pos provenance
                                 irritants merged-irritants badness)
           (missed-opt-log-entry kind msg stx located-stx pos provenance
                                 irritants merged-irritants
                                 ;; uses ceiling to never go down to 0
                                 ;; both badness and badness-multiplier are non-0
                                 (ceiling (* badness badness-multiplier)))]
          [_ l])))) ; keep as is
