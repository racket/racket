#lang racket/base

(require typed-racket/optimizer/logging)

(provide (struct-out report-entry)
         (struct-out sub-report-entry)
         (struct-out opt-report-entry)
         (struct-out missed-opt-report-entry)
         (struct-out inliner-log-entry)
         (struct-out inlining-event)
         ;; from typed-racket/optimizer/logging
         (struct-out log-entry)
         (struct-out opt-log-entry)
         (struct-out missed-opt-log-entry)
         (struct-out info-log-entry))


;; Similar to the log-entry family of structs, but geared towards GUI display.
;; Also designed to contain info for multiple overlapping log entries.
;; - subs is a list of sub-report-entry, corresponding to all the entries
;;   between start and end
;; - badness is 0 for a report-entry containing only optimizations
;;   otherwise, it's the sum for all the subs
(struct report-entry (subs start end badness))
;; multiple of these can be contained in a report-entry
;; provenance is one of: 'typed-racket 'mzc
(struct sub-report-entry (stx msg provenance))
(struct opt-report-entry        sub-report-entry ())
(struct missed-opt-report-entry sub-report-entry (badness irritants))


(struct inliner-log-entry log-entry (inlining-event) #:prefab)


(struct inlining-event (kind ; success, miss, out of fuel, ...
                        name ; _what_ gets inlined
                        loc  ; (U #f (List path line col pos span))
                        where-name ; _where_ it gets inlined (enclosing fun)
                        where-loc  ; (U #f (List path line col))
                        size ; size of the closure being inlined
                        threshold ; how big of a closure can we inline
                        ;; the last two use the same units
                        ))
