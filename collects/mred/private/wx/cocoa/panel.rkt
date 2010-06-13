#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
          "types.rkt"
          "utils.rkt"
         "window.rkt")
(unsafe!)
(objc-unsafe!)

(provide panel%
         panel-mixin)

(import-class NSView)

(define (panel-mixin %)
  (class %
    (define lbl-pos 'horizontal)
    (super-new)
    
    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (def/public-unimplemented on-paint)
    (define/public (set-item-cursor x y) (void))
    (def/public-unimplemented get-item-cursor)))

(defclass panel% (panel-mixin window%)
  (init parent
        x y w h
        style
        label)

  (super-new [parent parent]
             [cocoa
              (as-objc-allocation
               (tell (tell NSView alloc)
                     initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint x y)
                                                                (make-NSSize w h))))]
             [no-show? (memq 'deleted style)]))
  
