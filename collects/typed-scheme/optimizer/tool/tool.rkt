#lang racket/base

(require racket/class racket/port racket/list racket/match
         racket/gui/base racket/unit drracket/tool)

(require "report.rkt" "display.rkt")

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
        [(report-entry subs start end badness)
         (let ([color (if (= badness 0)
                          "lightgreen"
                          (vector-ref color-table badness))])
           (send this highlight-range start end color)
           (send this set-clickback start end (popup-callback l))
           ;; record highlights to undo them later
           (cons (list start end color)
                 ;; missed optimizations have irritants, circle them
                 (filter values ; remove irritants w/o location
                         (map highlight-irritant
                              (append-map missed-opt-report-entry-irritants
                                          (filter missed-opt-report-entry?
                                                  subs))))))]))

    (define/public (add-highlights)
      (define report (generate-report this))
      (define max-badness (apply max (map report-entry-badness report)))
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
