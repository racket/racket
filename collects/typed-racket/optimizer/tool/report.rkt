#lang racket/base

(require racket/class racket/gui/base racket/match racket/list
         unstable/syntax unstable/logging
         typed-racket/optimizer/logging
         "mzc.rkt" "sandbox.rkt")

(provide (struct-out report-entry)
         (struct-out sub-report-entry)
         (struct-out opt-report-entry)
         (struct-out missed-opt-report-entry)
         generate-report
         collapse-report)

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

;; profile is currently only used to refine the inlining logs
(define (generate-report this profile)
  (define-values (TR-log mzc-log) (generate-logs this))
  (log->report
   (append TR-log
           (post-process-inline-log mzc-log profile TR-log))))


(define (generate-logs this)
  (define file-predicate (make-file-predicate this))
  (define input          (open-input-text-editor this))
  (port-count-lines! input)
  (define (right-file? l) ; does the log-entry refer to the file we're in?
    (define stx (log-entry-stx l))
    (define path
      (let ([dir  (syntax-source-directory stx)]
            [file (syntax-source-file-name stx)])
        (if (and dir file)
            (build-path dir file)
            #f)))
    (file-predicate path))
  (define TR-log  '())
  (define mzc-log '())
  (with-intercepted-logging
      (lambda (l)
        ;; From mzc, create a log-entry from the info.
        (define entry (mzc-opt-log-message->log-entry (vector-ref l 1)))
        (when (right-file? entry)
          (set! mzc-log (cons entry mzc-log))))
    (lambda ()
      (with-intercepted-logging
          (lambda (l)
            ;; From TR, use the log-entry struct provided.
            (define entry (vector-ref l 2))
            (when (right-file? entry)
              (set! TR-log (cons entry TR-log))))
        (lambda ()
          (run-inside-optimization-coach-sandbox
           this
           (lambda ()
             (void (compile (read-syntax (send this get-port-name) input))))))
        'debug 'TR-optimizer))
    'debug 'optimizer)
  (values (reverse TR-log) (reverse mzc-log)))


;; converts log-entry structs to report-entry structs for further
;; processing
(define (log->report log)
  (define (log-entry->report-entry l)
    (match l
      [(? info-log-entry? _)
       ;; Info entries are only useful for log analysis, and should not be
       ;; presented to users. Drop them.
       #f]
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
  ;; We remove duplicates that were caused by traversing the same piece
  ;; of code multiple times in the optimizer.
  (filter values
          (map log-entry->report-entry
               ;; merge missed-opts hierarchically
               (for/fold ([res '()])
                   ([new (remove-duplicates log)])
                 (cond [(missed-opt-log-entry? new)
                        (maybe-merge-with-parent new res)]
                       [else
                        (cons new res)]))))) ; no merging for opts and info

;;--------------------------------------------------------------------

;; is parent the "parent" missed optimization of child?
;; this determines whether they get reported together or not
;; currently, parents and children must be of the same kind of missed
;; optimization, and the child must be an irritant of the parent, or be a
;; merged irritant of the parent
(define (parent-of? parent child)
  (and (missed-opt-log-entry? parent) ; only applicable for missed opts
       (missed-opt-log-entry? child)
       (equal? (log-entry-kind parent)
               (log-entry-kind child))
       (member (log-entry-stx child)
               (append (missed-opt-log-entry-irritants parent)
                       (missed-opt-log-entry-merged-irritants parent)))))

;; combine reporting of two missed optimizations, increasing badness in the
;; process
(define (combine-missed-optimizations parent child)
  (missed-opt-log-entry
   (log-entry-kind        parent) ; same as child's
   (log-entry-msg         parent)
   (log-entry-stx         parent) ; we report the outermost one
   (log-entry-located-stx parent)
   (log-entry-pos         parent)
   (log-entry-provenance  parent)

   (remove-duplicates
    (append (remove (log-entry-stx child)
                    (missed-opt-log-entry-irritants parent))
            (missed-opt-log-entry-irritants child)))
   (remove-duplicates
    (append (missed-opt-log-entry-merged-irritants child)
            (missed-opt-log-entry-merged-irritants parent)
            ;; we merge child in, keep it for future merges
            (list (log-entry-stx child))))
   (+ (missed-opt-log-entry-badness parent)
      (missed-opt-log-entry-badness child))))

;; log-entry (listof log-entry) -> log-entry
;; add a new missed opt to the list, maybe replacing its parent / children
(define (maybe-merge-with-parent new log-so-far)
  ;; check if the new one is the child of an old one
  ;; for/first is ok, since we can only have one parent in the list
  ;; (if we had more, one would have to be the parent of the other, so
  ;; only one would be in the list)
  (define parent (for/first ([m (in-list log-so-far)]
                             #:when (parent-of? m new))
                   m))
  ;; do we have children in the list, if so, merge with all of them
  (define children (for/list ([m (in-list log-so-far)]
                              #:when (parent-of? new m))
                     m))
  (cond [parent
         ;; we found our parent, merge with it
         (if (member (log-entry-stx new)
                     (missed-opt-log-entry-merged-irritants
                      parent))
             ;; we have been merged in the past, do nothing
             log-so-far
             ;; do the actual merge
             (cons (combine-missed-optimizations parent new)
                   (remove parent log-so-far)))]
        [(not (null? children))
         ;; we found children, merge with them
         (let ([new (for/fold ([new new])
                        ([child children])
                      (combine-missed-optimizations new child))])
           (cons new
                 (filter (lambda (x) (not (member x children)))
                         log-so-far)))]
        [else
         ;; no related entry, just add the new one
         (cons new log-so-far)]))

;;--------------------------------------------------------------------

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
