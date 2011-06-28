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

(define highlights '())

;; performance-report-callback : drracket:unit:frame<%> -> void
(define (performance-report-callback drr-frame)
  (define defs     (send drr-frame get-definitions-text)) ; : text%
  (define portname (send defs      get-port-name))
  (define input    (open-input-text-editor defs))
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
  (set! log (reverse log))
  (define (highlight-irritant i)
    (define pos (syntax-position i))
    (and pos
         (let ([start (sub1 pos)]
               [end   (sub1 (+ pos (syntax-span i)))]
               [color "red"]
               [caret-space #f]
               [style 'hollow-ellipse])
           ;; high priority, to display above the coloring
           (send defs highlight-range start end color caret-space 'high style)
           ;; info needed to remove the highlight
           (list start end color caret-space style))))
  ;; highlight
  (define new-highlights
    (for/list ([l (in-list log)])
      (match l
        [(log-entry kind msg stx (? number? pos))
         (let* ([start (sub1 pos)]
                [end   (+ start (syntax-span stx))]
                [opt?  (opt-log-entry? l)] ;; opt or missed opt?
                [color (if opt? "lightgreen" "pink")])
           (send defs highlight-range start end color)
           (send defs set-clickback start end
                 (lambda (ed start end)
                   (message-box "Performance Report" msg)))
           (cons (list start end color) ; record highlights to undo them later
                 ;; missed optimizations have irritants, circle them
                 (if opt?
                     '()
                     (filter values ; remove irritants w/o location
                             (map highlight-irritant
                                  (missed-opt-log-entry-irritants l))))))]
        [_ '()]))) ; no source location, don't highlight anything
  (set! highlights (append (apply append new-highlights) highlights)))

(define remove-highlights-mixin
  (mixin ((class->interface text%)) ()
    (inherit begin-edit-sequence
             end-edit-sequence
             insert
             get-text)
    (define (clear-highlights)
      (for ([h (in-list highlights)])
        (match h
          [`(,start ,end . ,rest )
           (send this unhighlight-range . h)
           (send this remove-clickback start end)])))
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
  (drracket:get/extend:extend-definitions-text remove-highlights-mixin))

(define performance-report-drracket-button
  (list
   "Performance Report"
   performance-report-bitmap
   performance-report-callback))
