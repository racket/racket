#lang racket/base

(require racket/class racket/gui/base racket/match)

(require (prefix-in tr: typed-scheme/typed-reader)
         typed-scheme/optimizer/logging)

(provide (struct-out report-entry)
         (struct-out opt-report-entry)
         (struct-out missed-opt-report-entry)
         generate-report)

;; Similar to the log-entry family of structs, but geared towards GUI display.
;; Also designed to contain info for multiple overlapping log entries.
;; stxs+msgs is a list of syntax-message pairs
(struct report-entry (stxs+msgs start end))
(struct opt-report-entry        report-entry ())
(struct missed-opt-report-entry report-entry (badness irritants))


(define (generate-report this)
  (collapse-report (log->report (generate-log this))))


(define (generate-log this)
  (define portname (send this get-port-name))
  (define input    (open-input-text-editor this))
  (port-count-lines! input)
  (define log '())
  (with-intercepted-tr-logging
   (lambda (l)
     (set! log (cons (cdr (vector-ref l 2)) ; log-entry struct
                     log)))
   (lambda ()
     (parameterize ([current-namespace  (make-base-namespace)]
                    [read-accept-reader #t])
       (expand (tr:read-syntax portname input)))))
  log)

;; converts log-entry structs to report-entry structs for further
;; processing
(define (log->report log)
  (define (log-entry->report-entry l)
    (match l
      [(log-entry kind msg stx located-stx (? number? pos))
       (define stxs+msgs `((,located-stx . ,msg)))
       (define start     (sub1 pos))
       (define end       (+ start (syntax-span stx)))
       (if (opt-log-entry? l)
           (opt-report-entry stxs+msgs start end)
           (missed-opt-report-entry stxs+msgs start end
                                    (missed-opt-log-entry-badness   l)
                                    (missed-opt-log-entry-irritants l)))]
      [_ #f])) ; no source location, ignore
  (filter values (map log-entry->report-entry log)))

(define (merge-entries prev l)
  (define new-stxs+msgs
    (append (report-entry-stxs+msgs prev) (report-entry-stxs+msgs l)))
  (match (list prev l)
    [`(,(missed-opt-report-entry ss+ms1 start1 end1 bad1 irr1)
       ,(missed-opt-report-entry ss+ms2 start2 end2 bad2 irr2))
     ;; we take start1 and end1 since prev includes l
     (missed-opt-report-entry new-stxs+msgs start1 end1
                              (+ bad1 bad2) (append irr1 irr2))]
    [(or `(,(missed-opt-report-entry ss+ms1 start1 end1 bad irr)
           ,(report-entry ss+ms2 start2 end2))
         `(,(report-entry ss+ms1 start1 end1)
           ,(missed-opt-report-entry ss+ms2 start2 end2 bad irr)))
     ;; since missed opts are more important to report, they win
     (missed-opt-report-entry new-stxs+msgs start1 end1 bad irr)]
    [`(,(report-entry ss+ms1 start1 end1) ,(report-entry ss+ms2 start2 end2))
     ;; both are opts
     (report-entry new-stxs+msgs start1 end1)]))

;; detect overlapping reports and merge them
(define (collapse-report orig-report)
  ;; sort in order of starting point
  (define report (sort orig-report < #:key report-entry-start))
  (define-values (new-report _)
    (for/fold ([new-report '()]
               [prev #f])
        ([l (in-list report)])
      (match* (prev l)
        [((report-entry stxs+msgs1 start1 end1)
          (report-entry stxs+msgs2 start2 end2))
         (=> unmatch)
         (if (< start2 end1) ; l in within prev
             ;; merge the two
             (values (cons (merge-entries prev l) (cdr new-report))
                     ;; we don't advance, since we merged
                     prev)
             (unmatch))]
        [(prev l) ; no overlap, just add to the list
         (values (cons l new-report) l)])))
  new-report)
