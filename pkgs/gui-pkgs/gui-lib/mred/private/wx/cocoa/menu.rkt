#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         (only-in racket/list drop take)
         "../common/event.rkt"
         "../../syntax.rkt"
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "window.rkt"
         "menu-item.rkt")

(provide 
 (protect-out menu%))

(import-class NSMenu NSMenuItem NSEvent)

(define-struct mitem (item checkable?))

(defclass menu% object%
  (init-field label
              callback
              font)

  (super-new)

  (define items null)

  (define cocoa #f)
  (define cocoa-menu #f)

  (define/public (create-menu label)
    (unless cocoa
      (set! cocoa
            (as-objc-allocation
             (tell (tell NSMenuItem alloc)
                   initWithTitle: #:type _NSString (clean-menu-label label)
                   action: #:type _SEL #f
                   keyEquivalent: #:type _NSString "")))
      (set! cocoa-menu
            (as-objc-allocation
             (tell (tell NSMenu alloc)
                   initWithTitle: #:type _NSString (clean-menu-label label))))
      (tellv cocoa-menu setAutoenablesItems: #:type _BOOL #f)
      (tellv cocoa setSubmenu: cocoa-menu)
      (for-each (lambda (item)
                  (if item
                      (send (mitem-item item) install cocoa-menu (mitem-checkable? item))
                      (tellv cocoa-menu addItem: (tell NSMenuItem separatorItem))))
                items)))

  (define/public (install cocoa-parent label enabled?)
    (create-menu label)
    (tellv cocoa-parent addItem: cocoa)
    (tellv cocoa setEnabled: #:type _BOOL enabled?))

  (define popup-box #f)

  (define/public (do-popup v win x y queue-cb)
    (unless (null? items)
      (create-menu "menu")
      (let ([b (box #f)])
        (set! popup-box b)
        (if (not (version-10.6-or-later?))
            ;; For 10.5 and earlier:
            (let ([p (tell #:type _NSPoint v
                           convertPoint: #:type _NSPoint (make-NSPoint x y)
                           toView: #f)])
              (atomically
               (with-autorelease
                (tellv NSMenu popUpContextMenu: cocoa-menu 
                       withEvent: (tell NSEvent 
                                        mouseEventWithType: #:type _int NSLeftMouseDown
                                        location: #:type _NSPoint p
                                        modifierFlags: #:type _NSUInteger 0 
                                        timestamp: #:type _double 0.0
                                        windowNumber: #:type _NSUInteger 
                                        (tell #:type _NSInteger win windowNumber)
                                        context: #:type _pointer #f
                                        eventNumber: #:type _NSInteger 0
                                        clickCount: #:type _NSInteger 1
                                        pressure: #:type _float 1.0)
                       forView: v))))
            ;; 10.6 and later:
            (tellv cocoa-menu 
                   popUpMenuPositioningItem: (tell cocoa-menu itemAtIndex: #:type _NSUInteger 0)
                   atLocation: #:type _NSPoint (make-NSPoint x y)
                   inView: v))
        (set! popup-box #f)
        (let* ([i (unbox b)]
               [e (new popup-event% [event-type 'menu-popdown])])
          (send e set-menu-id i)
          (queue-cb (lambda () (callback this e)))))))
  
  (define/public (item-selected menu-item)
    ;; called in Cocoa thread
    (cond
     [popup-box
      (set-box! popup-box menu-item)]
     [(parent . is-a? . menu%)
      (send parent item-selected menu-item)]
     [else
      (let ([top (get-top-parent)])
        (when top
          (queue-window-event
           top
           (lambda () (send top on-menu-command menu-item)))))]))

  (define parent #f)
  (define/public (set-parent p) (set! parent p))
  (define/public (get-top-parent)
    ;; called in Cocoa thread
    (and parent
         (if (parent . is-a? . menu%)
             (send parent get-top-parent)
             (send parent get-top-window))))

  (public [append-item append])
  (define (append-item i label help-str-or-submenu chckable?)
    (send i set-label label)
    (when (help-str-or-submenu . is-a? . menu%)
      (send i set-submenu help-str-or-submenu)
      (send help-str-or-submenu set-parent this))
    (set! items (append items (list (make-mitem i chckable?))))
    (send i set-parent this)
    (when cocoa-menu
      (send i install cocoa-menu chckable?)))

  (define/public (append-separator)
    (set! items (append items (list #f)))
    (when cocoa-menu
      (tellv cocoa-menu addItem: (tell NSMenuItem separatorItem))))

  (def/public-unimplemented select)
  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)

  (define/public (set-help-string m s) (void))

  (def/public-unimplemented number)

  (define/private (find-pos item)
    (for/or ([i (in-list items)]
             [pos (in-naturals)])
      (and i
           (eq? (mitem-item i) item) 
           pos)))

  (define/public (adjust item cocoa-cb cb)
    (let ([pos (find-pos item)])
      (when pos
        (when cocoa-menu
          (cocoa-cb (tell cocoa-menu itemAtIndex: #:type _NSInteger pos)))
        (cb (list-ref items pos)))))
  
  (define/public (set-label item label)
    (adjust item
            (lambda (item-cocoa)
              (tellv item-cocoa setTitle: #:type _NSString (clean-menu-label (regexp-replace #rx"\t.*" label "")))
              (set-menu-item-shortcut item-cocoa label))
            (lambda (mitem)
              (send (mitem-item mitem) set-label (clean-menu-label label)))))

  (define/public (check item on?)
    (adjust item
            (lambda (item-cocoa)
              (tellv item-cocoa setState: #:type _int (if on? 1 0)))
            (lambda (mitem)
              (send (mitem-item mitem) set-checked (and on? #t)))))
                  
  (define/public (enable item on?)
    (adjust item
            (lambda (item-cocoa)
              (tellv item-cocoa setEnabled: #:type _BOOL on?))
            (lambda (mitem)
              (send (mitem-item mitem) set-enabled-flag (and on? #t)))))
    
  (define/public (checked? item)
    (send item get-checked))

  (define/public (delete-by-position pos)
    (let ([mitem (list-ref items pos)])
      (set! items (append (take items pos)
                          (drop items (add1 pos))))
      (when cocoa-menu
        (tellv cocoa-menu removeItemAtIndex: #:type _NSInteger pos))))

  (define/public (delete item)
    (let ([pos (find-pos item)])
      (when pos
        (delete-by-position pos)))))
