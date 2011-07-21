#lang racket/unit

(require racket/class racket/gui/base racket/match
         unstable/syntax drracket/tool
         unstable/logging)

(require typed-scheme/optimizer/logging
         "report-sig.rkt" "report-structs.rkt")

(import drracket:tool^)
(export report^)


(define (generate-report this)
  (collapse-report (log->report (generate-log this))))


(define (generate-log this)
  (define portname (send this get-port-name))
  (define input    (open-input-text-editor this))
  (port-count-lines! input)
  (define log '())
  (define unsaved-file?
    (and (symbol? portname)
         (regexp-match #rx"^unsaved-editor" (symbol->string portname))))
  (define good-portname-cache #f)
  (define (right-file? f) ; does the log-entry refer to the file we're in?
    (cond [(and good-portname-cache ; cache is populated
                (equal? f good-portname-cache))
           #t]
          ;; no cache, ask directly
          [(send this port-name-matches? f)
           (set! good-portname-cache f) ; populate cache
           #t]
          [unsaved-file?
           ;; we assume that any log entry without a filename comes from
           ;; the unsaved editor
           (not f)]
          [else ; different file
           #f]))

  ;; expand and capture log messages
  (define listener #f)
  (define exception #f)
  (define done-chan (make-channel))
  (drracket:eval:expand-program
   (drracket:language:make-text/pos
    this 0 (send this last-position))
   (send this get-next-settings) #t
   (lambda () ; init
     (uncaught-exception-handler
      (lambda (e)
        (set! exception e) ; something went wrong, save exception and die
        (channel-put done-chan 'done) ; let the rest of the program carry on
        (custodian-shutdown-all (current-custodian)))) ; kill ourselves
     (set! listener (start-recording #:level 'warning)))
   (lambda () ; kill
     (channel-put done-chan 'done))
   (lambda (term k)
     (if (eof-object? term)
         (begin (set! log (stop-recording listener)) ; done, stash the log
                (channel-put done-chan 'done))
         (k)))) ; not done, keep going
  (channel-get done-chan) ; wait for expansion to finish

  (when exception ; something went wrong, will be caught upstream
    (raise exception))

  (define (post-process-log-entry l)
    ;; make sure the message is indeed from the optimizer
    (cond [(log-message-from-tr-opt? l)
           (define log-entry-data (cdr (vector-ref l 2))) ; log-entry struct
           (define stx (log-entry-stx log-entry-data))
           (define path (if (and (syntax-source-directory stx)
                                 (syntax-source-file-name stx))
                            (build-path (syntax-source-directory stx)
                                        (syntax-source-file-name stx))
                            #f))
           ;; it also needs to come from the right file
           (if (right-file? path)
               log-entry-data ; payload
               #f)]
          [else #f])) ; drop it

  (filter values (map post-process-log-entry log)))

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
