#lang racket/base

(require racket/class racket/port racket/list
         racket/gui/base mrlib/switchable-button
         racket/unit drracket/tool)

(require (prefix-in tr: typed-scheme/typed-reader)
         typed-scheme/optimizer/logging)

(provide performance-report-drracket-button
         tool@)

;; DrRacket tool for reporting missed optimizations in the editor.

(define reverse-content-bitmap
  (let* ((bmp (make-bitmap 16 16))
         (bdc (make-object bitmap-dc% bmp)))
    (send bdc erase)
    (send bdc set-smoothing 'smoothed)
    (send bdc set-pen "black" 1 'transparent)
    (send bdc set-brush "blue" 'solid)
    (send bdc draw-ellipse 2 2 8 8)
    (send bdc set-brush "red" 'solid)
    (send bdc draw-ellipse 6 6 8 8)
    (send bdc set-bitmap #f)
    bmp))

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
  ;; highlight
  (define new-highlights
    (for/list ([l (in-list log)])
      (let* ([stx (log-entry-stx l)]
             [pos (sub1 (log-entry-pos l))]
             [end (+ pos (syntax-span stx))]
             ;; opt or missed opt?
             [opt? (regexp-match #rx"^TR opt:" (log-entry-msg l))]
             [color (if opt? "lightgreen" "pink")])
        (send defs highlight-range pos end color)
        (list pos end color)))) ; record the highlight, to undo it later
  (set! highlights (append new-highlights highlights))
  (message-box
   "Performance Report"
   (with-output-to-string
    (lambda ()
      (for ([l (in-list log)])
        (displayln (log-entry-msg l)))))))

(define remove-highlights-mixin
  (mixin ((class->interface text%)) ()
    (inherit begin-edit-sequence
             end-edit-sequence
             insert
             get-text)
    (define (clear-highlights)
      (for ([h (in-list highlights)])
        (send this unhighlight-range . h)))
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
   reverse-content-bitmap
   performance-report-callback))
