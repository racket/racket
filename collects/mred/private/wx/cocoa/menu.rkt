#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
          "utils.rkt"
          "types.rkt")
(unsafe!)
(objc-unsafe!)

(provide menu%)

(import-class NSMenu NSMenuItem)

(define-struct mitem (item 
                      [label #:mutable]
                      [checked? #:mutable]
                      [enabled? #:mutable]))

(defclass menu% object%
  (init-field label
              callback
              font)

  (super-new)

  (define items null)

  (define cocoa #f)
  (define cocoa-menu #f)

  (define/public (install cocoa-parent label)
    (unless cocoa
      (set! cocoa
            (as-objc-allocation
             (tell (tell NSMenuItem alloc)
                   initWithTitle: #:type _NSString label
                   action: #:type _SEL #f
                   keyEquivalent: #:type _NSString "")))
      (set! cocoa-menu
            (as-objc-allocation
             (tell (tell NSMenu alloc)
                   initWithTitle: #:type _NSString label)))
      (tellv cocoa setSubmenu: cocoa-menu)
      (for-each (lambda (item)
                  (if item
                      (send (mitem-item item) install cocoa-menu (mitem-label item))
                      (tellv cocoa-menu addItem: (tell NSMenuItem separatorItem))))
                items))
    (tellv cocoa-parent addItem: cocoa))

  (public [append-item append])
  (define (append-item i label help-str chckable?)
    (set! items (append items (list (make-mitem i label #f #f))))
    (when cocoa-menu
      (send i install cocoa-menu label)))

  (define/public (append-separator)
    (set! items (append items (list #f)))
    (when cocoa-menu
      (tellv cocoa-menu addItem: (tell NSMenuItem separatorItem))))

  (def/public-unimplemented select)
  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)

  (def/public-unimplemented set-help-string)
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
              (tellv item-cocoa setTitle: #:type _NSString label))
            (lambda (mitem)
              (set-mitem-label! mitem label))))
                  
  (define/public (check item on?)
    (adjust item
            (lambda (item-cocoa)
              (tellv item-cocoa setState: #:type _int (if on? 1 0)))
            (lambda (mitem)
              (set-mitem-checked?! mitem (and on? #t)))))
                  
  (define/public (enable item on?)
    (adjust item
            (lambda (item-cocoa)
              (tellv item-cocoa setEnabled: #:type _BOOL on?))
            (lambda (mitem)
              (set-mitem-enabled?! mitem (and on? #t)))))
    
  (def/public-unimplemented checked?)
  (def/public-unimplemented delete-by-position)
  (def/public-unimplemented delete))
