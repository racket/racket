#lang racket/base

(require racket/gui/base racket/class)

(provide snip-canvas%)

(define snip-canvas%
  (class editor-canvas%
    (init parent
          make-snip
          [style null]
          [label #f]
          [horizontal-inset 5]
          [vertical-inset 5]
          [enabled #t]
          [vert-margin 0]
          [horiz-margin 0]
          [min-width 0]
          [min-height 0]
          [stretchable-width #t]
          [stretchable-height #t])
    
    (define snip #f)
    (define text (new read-only-text%))
    (send text set-writable #f)
    
    (define/public (get-snip) snip)
    
    (define/override (on-size w h)
      (update-snip w h)
      (super on-size w h))
    
    (define (update-snip w h)
      (define snip-w (max 0 (- w (* 2 horizontal-inset))))
      (define snip-h (max 0 (- h (* 2 vertical-inset))))
      (cond [snip  (send snip resize snip-w snip-h)]
            [else  (set-snip (make-snip snip-w snip-h))]))
    
    (define (set-snip s)
      (unless (is-a? s snip%)
        (raise-type-error 'set-snip "snip%" s))
      (set! snip s)
      (send text set-writable #t)
      (send text erase)
      (send text insert snip)
      (send text set-writable #f))
    
    (super-new [parent parent]
               [editor text]
               [horizontal-inset horizontal-inset]
               [vertical-inset vertical-inset]
               [label label]
               [enabled enabled]
               [style (list* 'no-hscroll 'no-vscroll style)]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width stretchable-width]
               [stretchable-height stretchable-height])))

(define read-only-text%
  (class text%
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
    
    (super-new)
    (send this hide-caret #t)))
