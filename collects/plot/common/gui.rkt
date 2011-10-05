#lang racket/base

;; Extra GUI classes.

(require racket/gui/base racket/class)

(provide (all-defined-out))

(define read-only-text%
  (class text%
    (super-new)
    
    (send this hide-caret #t)
    
    (define writable? #t)
    (define/public (set-writable w?) (set! writable? w?))
    
    (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? start len) writable?)
    (define/augment (can-insert? start len) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all)  #t]
        [else    writable?]))
    ))

(define (make-snip-frame snip width height label)
  (define frame (new frame% [width width] [height height] [label label]))
  (define text (new read-only-text%))
  (define canvas (new editor-canvas% [parent frame] [editor text]
                      [horizontal-inset 0] [vertical-inset 0] [horiz-margin 0] [vert-margin 0]
                      [enabled #t] [style '(no-hscroll no-vscroll no-border)]))
  (send text insert snip)
  (send text set-writable #f)
  frame)

(define (snip->canvas snip)
  (let/ec return
    (define admin (send snip get-admin))
    (when (not admin) (return #f))
    (define editor (send admin get-editor))
    (send editor get-active-canvas)))
