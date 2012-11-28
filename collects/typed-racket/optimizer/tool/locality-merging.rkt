#lang racket/base

;; Only includes pitfall-agnostic locality merging (the one that doesn't
;; generate new information).
;; Pitfall-specific locality merging is done in the pitfall's file.

(require racket/match
         "structs.rkt")

(provide locality-merging)

(define (merge-entries prev l)
  (match* (prev l)
    [((report-entry subs1 start1 end1 badness1)
      (report-entry subs2 start2 end2 badness2))
     (report-entry (append subs1 subs2)
                   start1 end1 ; prev includes l
                   (+ badness1 badness2))]))

;; detect overlapping reports and merge them
(define (locality-merging orig-report)
  ;; sort in order of starting point
  (define report (sort orig-report < #:key report-entry-start))
  (define-values (new-report _)
    (for/fold ([new-report '()]
               [prev #f])
        ([l (in-list report)])
      (match* (prev l)
        [((report-entry subs1 start1 end1 badness1)
          (report-entry subs2 start2 end2 badness2))
         (=> unmatch)
         (if (< start2 end1) ; l in within prev
             ;; merge the two
             (let ([merged (merge-entries prev l)])
               (values (cons merged (cdr new-report))
                       merged))
             (unmatch))]
        [(prev l) ; no overlap, just add to the list
         (values (cons l new-report) l)])))
  new-report)
