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
    (inherit register-as-child)

    (define lbl-pos 'horizontal)
    (define children null)

    (super-new)
    
    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (define/public (fix-dc)
      (for ([child (in-list children)])
        (send child fix-dc)))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (fix-dc))
    
    (define/override (maybe-register-as-child parent on?)
      (register-as-child parent on?))
    
    (define/override (register-child child on?)
      (let ([now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children))))))
    
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
