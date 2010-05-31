#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
          "types.rkt"
          "utils.rkt"
          "window.rkt"
          "panel.rkt")
(unsafe!)
(objc-unsafe!)

(provide tab-panel%)

(import-class NSView NSTabView NSTabViewItem)

(defclass tab-panel% (panel-mixin window%)
  (init parent
        x y w h
        style
        labels)
  (inherit get-cocoa)

  (define cocoa (as-objc-allocation
                 (tell (tell NSTabView alloc) init)))
  (define item-cocoas
    (for/list ([lbl (in-list labels)])
      (let ([item (as-objc-allocation
                   (tell (tell NSTabViewItem alloc) initWithIdentifier: #f))])
        (tellv item setLabel: #:type _NSString lbl)
        (tellv cocoa addTabViewItem: item)
        item)))
  (let ([sz (tell #:type _NSSize cocoa minimumSize)])
    (tellv cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint x y) sz)))
  
  (define content-cocoa 
    (as-objc-allocation
     (tell (tell NSView alloc)
           initWithFrame: #:type _NSRect (tell #:type _NSRect cocoa contentRect))))
  (tell #:type _void cocoa addSubview: content-cocoa)

  (define/override (get-cocoa-content) content-cocoa)
  (define/override (set-size x y w h)
    (super set-size x y w h)
    (tellv content-cocoa setFrame: #:type _NSRect (tell #:type _NSRect cocoa contentRect)))

  (define/public (set-label i str)
    (tellv (list-ref item-cocoas i) setLabel: #:type _NSString str))

  (define/public (set-selection i)
    (tellv cocoa selectTabViewItem: (list-ref item-cocoas i)))

  (super-new [parent parent]
             [cocoa cocoa]
             [no-show? (memq 'deleted style)]))
