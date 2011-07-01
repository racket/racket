#lang racket/base

(require racket/class racket/port racket/list racket/match racket/string
         racket/gui/base mrlib/switchable-button
         racket/unit drracket/tool
         unstable/sequence)

(require (prefix-in tr: typed-scheme/typed-reader)
         typed-scheme/optimizer/logging)

(provide performance-report-drracket-button
         tool@)

;; DrRacket tool for reporting missed optimizations in the editor.

(define performance-report-bitmap
  (make-object
   bitmap%
   (collection-file-path "performance-report.png" "icons") 'png/mask))

;; performance-report-callback : drracket:unit:frame<%> -> void
(define (performance-report-callback drr-frame)
  (send (send drr-frame get-definitions-text) add-highlights))

(define lowest-badness-color  (make-object color% "pink"))
(define highest-badness-color (make-object color% "red"))
;; the higher the badness, the closer to red the highlight should be
(define (make-color-table max-badness)
  (define min-g (send highest-badness-color green))
  (define max-g (send lowest-badness-color  green))
  (define min-b (send highest-badness-color blue))
  (define max-b (send lowest-badness-color  blue))
  (define delta-g (- max-g min-g))
  (define delta-b (- max-b min-b))
  (define bucket-size-g (quotient delta-g max-badness))
  (define bucket-size-b (quotient delta-b max-badness))
  (build-vector (add1 max-badness) ; to index directly using badness
                (lambda (x)
                  (make-object
                   color%
                   255
                   ;; clipping, since the first (unused, for
                   ;; badness of 0) would have invalid components
                   (min 255 (- max-g (* (sub1 x) bucket-size-g)))
                   (min 255 (- max-b (* (sub1 x) bucket-size-b)))))))

;; Similar to the log-entry family of structs, but geared towards GUI display.
;; Also designed to contain info for multiple overlapping log entries.
;; stxs+msgs is a list of syntax-message pairs
(struct report-entry (stxs+msgs start end))
(struct opt-report-entry        report-entry ())
(struct missed-opt-report-entry report-entry (badness irritants))

(define highlights-mixin
  (mixin ((class->interface text%)) ()
    (inherit begin-edit-sequence
             end-edit-sequence
             insert
             get-text)

    (define highlights  '())
    (define color-table #f)

    (define (highlight-irritant i)
      (define pos (syntax-position i))
      (and pos
           (let ([start (sub1 pos)]
                 [end   (sub1 (+ pos (syntax-span i)))]
                 [color "red"]
                 [caret-space #f]
                 [style 'hollow-ellipse])
             ;; high priority, to display above the coloring
             (send this highlight-range
                   start end color caret-space 'high style)
             ;; info needed to remove the highlight
             (list start end color caret-space style))))

    (define (format-message stxs+msgs)
      (string-join (for/list ([(stx msg) (in-pairs stxs+msgs)])
                     (format "~a\n~a" (syntax->datum stx) msg))
                   "\n\n"))

    (define (highlight-entry l)
      (match l
        [(report-entry stxs+msgs start end)
         (let* ([opt?  (opt-report-entry? l)] ; opt or missed opt?
                [color (if opt?
                           "lightgreen"
                           (vector-ref color-table
                                       (missed-opt-report-entry-badness l)))])
           (send this highlight-range start end color)
           (send this set-clickback start end
                 (lambda (ed start end)
                   (message-box "Performance Report"
                                (format-message stxs+msgs))))
           ;; record highlights to undo them later
           (cons (list start end color)
                 ;; missed optimizations have irritants, circle them
                 (if opt?
                     '()
                     (filter values ; remove irritants w/o location
                             (map highlight-irritant
                                  (missed-opt-report-entry-irritants l))))))]))

    (define (generate-log)
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
          [(log-entry kind msg stx (? number? pos))
           (define stxs+msgs `((,stx . ,msg)))
           (define start     (sub1 pos))
           (define end       (+ start (syntax-span stx)))
           (if (opt-log-entry? l)
               (opt-report-entry stxs+msgs start end)
               (missed-opt-report-entry stxs+msgs start end
                                        (missed-opt-log-entry-badness   l)
                                        (missed-opt-log-entry-irritants l)))]
          [_ #f])) ; no source location, ignore
      (filter values (map log-entry->report-entry log)))

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

    (define/public (add-highlights)
      (define report (collapse-report (log->report (generate-log))))
      (define max-badness
        (for/fold ([max-badness 0])
            ([l (in-list report)]
             #:when (missed-opt-report-entry? l))
          (max max-badness (missed-opt-report-entry-badness l))))
      (unless (= max-badness 0) ; no missed opts, color table code would error
        (set! color-table (make-color-table max-badness)))
      (define new-highlights (map highlight-entry report))
      (set! highlights (append (apply append new-highlights) highlights)))

    (define (clear-highlights)
      (for ([h (in-list highlights)])
        (match h
          [`(,start ,end . ,rest )
           (send this unhighlight-range . h)
           (send this remove-clickback start end)]))
      (set! highlights '()))

    (define/augment (after-insert start len)
      (clear-highlights))
    (define/augment (after-delete start len)
      (clear-highlights))

    (super-new)))

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

(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  (define (phase1) (void))
  (define (phase2) (void))
  (drracket:get/extend:extend-definitions-text highlights-mixin))

(define performance-report-drracket-button
  (list
   "Performance Report"
   performance-report-bitmap
   performance-report-callback))
