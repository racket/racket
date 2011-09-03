#lang racket/base

(require racket/class racket/gui/base racket/match racket/port
         unstable/syntax unstable/port racket/sandbox
         typed-racket/optimizer/logging
         (prefix-in tr: typed-racket/typed-reader))

(provide (struct-out report-entry)
         (struct-out sub-report-entry)
         (struct-out opt-report-entry)
         (struct-out missed-opt-report-entry)
         generate-report)

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

(define (generate-report this)
  (collapse-report (log->report (generate-log this))))


(define (generate-log this)
  (define portname (send this get-port-name))
  (define input    (open-input-text-editor this))
  (port-count-lines! input)
  (define unsaved-file?
    (and (symbol? portname)
         (regexp-match #rx"^unsaved-editor" (symbol->string portname))))
  (define good-portname-cache #f)
  (define (right-file? l) ; does the log-entry refer to the file we're in?
    (define stx (log-entry-stx l))
    (define path
      (let ([dir  (syntax-source-directory stx)]
            [file (syntax-source-file-name stx)])
        (if (and dir file)
            (build-path dir file)
            #f)))
    (cond [(and good-portname-cache ; cache is populated
                (equal? path good-portname-cache))
           #t]
          [good-portname-cache ; cache is populated, but we have the wrong file
           #f]
          [unsaved-file?
           ;; we assume that any log entry without a filename comes from
           ;; the unsaved editor
           (not path)]
          ;; no cache, ask directly
          [(send this port-name-matches? path)
           (set! good-portname-cache path) ; populate cache
           #t]
          [else ; different file
           #f]))
  (define log '())
  (define sandbox
    (parameterize ([sandbox-make-code-inspector current-code-inspector]
                   [sandbox-eval-limits         #f])
      (make-evaluator 'racket/base)))
  (call-in-sandbox-context sandbox
   (lambda ()
     (with-intercepted-tr-logging
      (lambda (l)
        (define data (cdr (vector-ref l 2))) ; get the log-entry part
        (set! log (cons data log)))
      (lambda ()
        (parameterize ([current-namespace  (make-base-namespace)]
                       [read-accept-reader #t])
          (void (expand (tr:read-syntax portname input))))))))
  (filter right-file? (reverse log)))

;; converts log-entry structs to report-entry structs for further
;; processing
(define (log->report log)
  (define (log-entry->report-entry l)
    (match l
      [(log-entry kind msg stx located-stx (? number? pos))
       (define start     (sub1 pos))
       (define end       (+ start (syntax-span stx)))
       (report-entry (list (if (opt-log-entry? l)
                               (opt-report-entry located-stx msg)
                               (missed-opt-report-entry
                                located-stx msg
                                (missed-opt-log-entry-badness   l)
                                (missed-opt-log-entry-irritants l))))
                     start end
                     (if (opt-log-entry? l) ; badness
                         0
                         (missed-opt-log-entry-badness l)))]
      [_ #f])) ; no source location, ignore
  (filter values (map log-entry->report-entry log)))

(define (merge-entries prev l)
  (match* (prev l)
    [((report-entry subs1 start1 end1 badness1)
      (report-entry subs2 start2 end2 badness2))
     (report-entry (append subs1 subs2)
                   start1 end1 ; prev includes l
                   (+ badness1 badness2))]))

;; detect overlapping reports and merge them
(define (collapse-report orig-report)
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
