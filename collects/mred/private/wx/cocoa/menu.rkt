#lang scheme/base
(require scheme/class
         scheme/foreign
         (only-in scheme/list drop take)
         ffi/objc
          "../../syntax.rkt"
          "utils.rkt"
          "types.rkt"
          "window.rkt")
(unsafe!)
(objc-unsafe!)

(provide menu%)

(import-class NSMenu NSMenuItem)

(define-struct mitem (item))

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
      (tellv cocoa-menu setAutoenablesItems: #:type _BOOL #f)
      (tellv cocoa setSubmenu: cocoa-menu)
      (for-each (lambda (item)
                  (if item
                      (send (mitem-item item) install cocoa-menu)
                      (tellv cocoa-menu addItem: (tell NSMenuItem separatorItem))))
                items))
    (tellv cocoa-parent addItem: cocoa))

  (define/public (item-selected menu-item)
    ;; called in Cocoa thread
    (let ([top (get-top-parent)])
      (when top
        (queue-window-event
         top
         (lambda () (send top on-menu-command menu-item))))))

  (define parent #f)
  (define/public (set-parent p) (set! parent p))
  (define/public (get-top-parent)
    ;; called in Cocoa thread
    (and parent
         (if (parent . is-a? . menu%)
             (send parent get-top-parent)
             (send parent get-top-window))))

  (public [append-item append])
  (define (append-item i label help-str chckable?)
    (send i set-label label)
    (set! items (append items (list (make-mitem i))))
    (send i set-parent this)
    (when cocoa-menu
      (send i install cocoa-menu)))

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
              (send (mitem-item mitem) set-label label))))
                  
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
