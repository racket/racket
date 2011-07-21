#lang racket/base

(provide (struct-out report-entry)
         (struct-out sub-report-entry)
         (struct-out opt-report-entry)
         (struct-out missed-opt-report-entry))

;; Similar to the log-entry family of structs, but geared towards GUI display.
;; Also designed to contain info for multiple overlapping log entries.
;; - subs is a list of sub-report-entry, corresponding to all the entries
;;   between start and end
;; - badness is 0 for a report-entry containing only optimizations
;;   otherwise, it's the sum for all the subs
(struct report-entry (subs start end badness))
;; multiple of these can be contained in a report-entry
(struct sub-report-entry (stx msg))
(struct opt-report-entry        sub-report-entry ())
(struct missed-opt-report-entry sub-report-entry (badness irritants))
