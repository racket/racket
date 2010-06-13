#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
          "types.rkt"
          "utils.rkt"
          "window.rkt"
          "panel.rkt"
          "../common/event.rkt"
          "../common/procs.rkt")
(unsafe!)
(objc-unsafe!)

(provide tab-panel%)

(import-class NSView NSTabView NSTabViewItem)
(import-protocol NSTabViewDelegate)

(define-objc-class MyTabView NSTabView
  #:protocols (NSTabViewDelegate)
  [wx]
  (-a _void (tabView: [_id cocoa] didSelectTabViewItem: [_id item-cocoa])
      (queue-window-event wx (lambda () (send wx do-callback)))))

(defclass tab-panel% (panel-mixin window%)
  (init parent
        x y w h
        style
        labels)
  (inherit get-cocoa)

  (define cocoa (as-objc-allocation
                 (tell (tell MyTabView alloc) init)))
  (define item-cocoas
    (for/list ([lbl (in-list labels)])
      (let ([item (as-objc-allocation
                   (tell (tell NSTabViewItem alloc) initWithIdentifier: #f))])
        (tellv item setLabel: #:type _NSString (label->plain-label lbl))
        (tellv cocoa addTabViewItem: item)
        item)))
  (let ([sz (tell #:type _NSSize cocoa minimumSize)])
    (tellv cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint x y) sz)))
  (tellv cocoa setDelegate: cocoa)
  
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
    (tellv (list-ref item-cocoas i) setLabel: #:type _NSString (label->plain-label str)))
  
  (define/public (set-selection i)
    (tellv cocoa selectTabViewItem: (list-ref item-cocoas i)))
  (define/public (get-selection)
    (item->index (tell cocoa selectedTabViewItem)))

  (define (item->index tv)
    (for/or ([c (in-list item-cocoas)]
             [i (in-naturals)])
      (and (ptr-equal? c tv) i)))

  (public [append* append])
  (define (append* lbl)
    (let ([item (as-objc-allocation
                 (tell (tell NSTabViewItem alloc) initWithIdentifier: #f))])
      (tellv item setLabel: #:type _NSString (label->plain-label lbl))
      (tellv cocoa addTabViewItem: item)
      (set! item-cocoas (append item-cocoas (list item)))))

  (define/public (delete i)
    (let ([item-cocoa (list-ref item-cocoas i)])
      (tellv cocoa removeTabViewItem: item-cocoa)
      (set! item-cocoas (remq item-cocoa item-cocoas))))

  (define/public (set choices)
    (for ([item-cocoa (in-list item-cocoas)])
      (tellv cocoa removeTabViewItem: item-cocoa))
    (set! item-cocoas null)
    (for ([lbl (in-list choices)])
      (append* lbl)))

  (define callback void)
  (define/public (set-callback cb) (set! callback cb))
  (define/public (do-callback)
    (callback this (new control-event%
                        [event-type 'tab-panel]
                        [time-stamp (current-milliseconds)])))

  (super-new [parent parent]
             [cocoa cocoa]
             [no-show? (memq 'deleted style)]))
