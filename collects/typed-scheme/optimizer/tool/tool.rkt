#lang racket/base

(require racket/class racket/port racket/list racket/match
         racket/gui/base mrlib/switchable-button
         racket/unit drracket/tool)

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

    (define (highlight-entry l)
      (match l
        [(log-entry kind msg stx (? number? pos))
         (let* ([start (sub1 pos)]
                [end   (+ start (syntax-span stx))]
                [opt?  (opt-log-entry? l)] ;; opt or missed opt?
                [color (if opt?
                           "lightgreen"
                           (vector-ref color-table
                                       (missed-opt-log-entry-badness l)))])
           (send this highlight-range start end color)
           (send this set-clickback start end
                 (lambda (ed start end)
                   (message-box "Performance Report" msg)))
           ;; record highlights to undo them later
           (cons (list start end color)
                 ;; missed optimizations have irritants, circle them
                 (if opt?
                     '()
                     (filter values ; remove irritants w/o location
                             (map highlight-irritant
                                  (missed-opt-log-entry-irritants l))))))]
        [_ '()])) ; no source location, don't highlight anything

    (define/public (add-highlights)
      (define portname (send this get-port-name))
      (define input    (open-input-text-editor this))
      (port-count-lines! input)
      (define log '())
      ;; generate the log
      (with-intercepted-tr-logging
       (lambda (l)
         (set! log (cons (cdr (vector-ref l 2)) ; log-entry struct
                         log)))
       (lambda ()
         (parameterize ([current-namespace  (make-base-namespace)]
                        [read-accept-reader #t])
           (expand (tr:read-syntax portname input)))))
      (set! log (sort log < #:key log-entry-pos))
      ;; detect overlapping reports
      (define-values (rev-log/overlaps _)
        (for/fold ([rev-log/overlaps '()]
                   [prev #f])
            ([l (in-list log)])
          (match* (prev l)
            [((log-entry k1 msg1 stx1 pos1) (log-entry k2 msg2 stx2 pos2))
             (=> unmatch)
             (define end-prev (+ pos1 (syntax-span stx1)))
             (if (< pos2 end-prev) ; l in within prev
                 ;; merge the two
                 (values (cons (merge-entries prev l)
                               (cdr rev-log/overlaps))
                         ;; we don't advance, since we merged
                         prev)
                 (unmatch))]
            [(l1 l2) ; no overlap, just add to the list
             (values (cons l rev-log/overlaps) l)])))
      (set! log rev-log/overlaps)
      (define max-badness
        (for/fold ([max-badness 0])
            ([l (in-list log)]
             #:when (missed-opt-log-entry? l))
          (max max-badness (missed-opt-log-entry-badness l))))
      (unless (= max-badness 0) ; no missed opts, color table code would error
        (set! color-table (make-color-table max-badness)))
      (define new-highlights (map highlight-entry log))
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
  (define new-stx (log-entry-stx prev)) ; prev starts earlier than l
  (define new-pos (log-entry-pos prev))
  (define new-msg (string-append (log-entry-msg prev) "\n" (log-entry-msg l)))
  (match (list prev l)
    [`(,(missed-opt-log-entry k1 m1 s1 p1 irritants1 m-irr1 badness1)
       ,(missed-opt-log-entry k2 m2 s2 p2 irritants2 m-irr2 badness2))
     ;; both are missed opts
     (missed-opt-log-entry #f ; kind doesn't matter at this point
                           new-msg new-stx new-pos
                           (append irritants1 irritants2)
                           #f ; merged-irritants either
                           (+ badness1 badness2))]
    [(or `(,(missed-opt-log-entry k1 m1 s1 p1 irritants m-irr badness)
           ,(log-entry k2 m2 s2 p2))
         `(,(log-entry k1 m1 s1 p1)
           ,(missed-opt-log-entry k2 m2 s2 p2 irritants m-irr badness)))
     ;; since missed opts are more important to report, they win
     (missed-opt-log-entry #f new-msg new-stx new-pos irritants #f badness)]
    [`(,(log-entry k1 m1 s1 p1) ,(log-entry k2 m2 s2 p2))
     ;; both are opts
     (log-entry #f new-msg new-stx new-pos)]))

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
