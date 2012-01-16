#lang racket/base

(require racket/gui racket/class string-constants)

(provide bitmap-canvas% logo-about-dialog%)

(define bitmap-canvas%
  (class canvas%
    (init parent)
    (init-field bitmap)
    (init [enabled #t] [vert-margin 0] [horiz-margin 0])
    
    (inherit get-dc refresh min-width min-height)
    
    (super-new [parent parent]
               [enabled enabled]
               [vert-margin vert-margin]
               [horiz-margin horiz-margin]
               [stretchable-width #f]
               [stretchable-height #f]
               [style '(transparent no-focus)])
    
    (min-width (send bitmap get-width))
    (min-height (send bitmap get-height))
    
    (define/public (set-bitmap new-bitmap)
      (set! bitmap new-bitmap)
      (min-width (send bitmap get-width))
      (min-height (send bitmap get-height))
      (refresh))
    
    (define/override (on-paint) 
      (send (get-dc) draw-bitmap bitmap 0 0))
    ))

(define message-text%
  (class text%
    (init messages)
    (super-new [auto-wrap #t])
    
    (define writable? #t)
    (define/augment (can-change-style? start len) writable?)
    (define/augment (can-delete? start len) writable?)
    (define/augment (can-insert? start len) writable?)
    (define/augment (can-load-file? filename format) writable?)
    (define/augment (can-save-file? filename format) writable?)
    (define/override (can-do-edit-operation? op [recursive? #t])
      (case op
        [(copy select-all)  #t]
        [else    writable?]))
    
    (for ([message  (in-list messages)])
      (send this insert message))
    (set! writable? #f)))

(define message-canvas%
  (class editor-canvas%
    (init parent messages [horiz-margin 0] [vert-margin 0])
    (super-new [parent parent]
               [editor (new message-text% [messages messages])]
               [horizontal-inset 0] [vertical-inset 0]
               [horiz-margin 0] [vert-margin 0]
               [enabled #t] [style '(auto-vscroll auto-hscroll no-border transparent)])))

(define logo-about-dialog%
  (class dialog%
    (init label parent bitmap messages [width 640] [height 200] [enabled #t])
    (super-new [label label]
               [parent parent]
               [width width]
               [height height]
               [enabled enabled]
               [spacing 10]
               [border 10])
    
    (define top-panel
      (new horizontal-panel% [parent this] [alignment '(center top)] [spacing 20]))
    
    (define bitmap-canvas
      (new bitmap-canvas% [parent top-panel] [bitmap bitmap]))
    
    (define message-canvas
      (new message-canvas% [parent top-panel] [messages messages]))
    
    (define close-button
      (new button%
           [label (string-constant close)]
           [parent this]
           [callback (λ (_1 _2)
                       (when (send this can-close?)
                         (send this on-close)
                         (send this show #f)))]
           [style '(border)]))
    
    (send close-button focus)
    ))

