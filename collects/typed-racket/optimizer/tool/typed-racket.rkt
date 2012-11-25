#lang racket/base

;; Typed Racket-specific optimization analysis.

(require racket/match
         "structs.rkt" "causality-merging.rkt" "profiling.rkt")

(provide report-typed-racket)

(define (report-typed-racket TR-log profile hot-functions)
  (log->report
   (causality-merging
    (prune-cold-TR-failures TR-log profile hot-functions))))

;; Returns a report-entry or #f, which means prune.
(define (log-entry->report-entry l)
  (match l
    [(log-entry kind msg stx located-stx (? number? pos))
     (define start     (sub1 pos))
     (define end       (+ start (syntax-span stx)))
     (define provenance 'typed-racket)
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

(define (prune-cold-TR-failures TR-log profile hot-functions)
  (define total-time (and profile (profile-total-time profile)))

  ;; #f if no profiling info is available for this function
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
          [(missed-opt-log-entry kind msg stx located-stx pos
                                 irritants merged-irritants badness)
           (missed-opt-log-entry kind msg stx located-stx pos
                                 irritants merged-irritants
                                 ;; uses ceiling to never go down to 0
                                 ;; both badness and badness-multiplier are non-0
                                 (ceiling (* badness badness-multiplier)))]
          [_ l])))) ; keep as is
