#lang racket/base

;; GUI helpers

(require racket/gui/base racket/class unstable/gui/snip)

(provide (all-defined-out))

(define snip-frame%
  (class frame%
    (super-new)
    
    (define/override (on-traverse-char event)
      (define key-code (send event get-key-code))
      (case key-code
        [(escape)  (send this show #f)]
        [else  (super on-traverse-char event)]))
    ))

(define (make-snip-frame snip width height label)
  (define (make-snip w h)
    (send snip resize w h)
    snip)
  
  (define frame
    (new snip-frame% [label label] [width (+ 10 width)] [height (+ 10 height)]))
  
  (new snip-canvas%
       [parent frame]
       [make-snip make-snip]
       [horiz-margin 5] [vert-margin 5]
       [horizontal-inset 5] [vertical-inset 5])
  
  frame)
