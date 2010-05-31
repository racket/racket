#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
         "../../syntax.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")
(unsafe!)
(objc-unsafe!)

(provide menu-bar%)

(import-class NSApplication NSMenu NSMenuItem NSProcessInfo)

(define-cf CFBundleGetMainBundle (_fun -> _pointer))
(define-cf CFBundleGetInfoDictionary (_fun _pointer -> _id))

(define app-name
  (or
   (let ([dict (CFBundleGetInfoDictionary (CFBundleGetMainBundle))])
     (and dict
          (let ([appName (tell dict objectForKey: #:type _NSString "CFBundleName")]
                [alt (lambda ()
                       (tell #:type _NSString (tell NSProcessInfo processInfo) processName))])
            (if (not appName)
                (alt)
                (let ([appName (cast appName _id _NSString)])
                  (if (equal? appName "")
                      (alt)
                      appName))))))
   "MrEd"))

(define cocoa-mb (tell (tell NSMenu alloc) init))

;; Init menu bar
(let ([app (tell NSApplication sharedApplication)]
      [add-one (lambda (mb menu)
                 (let ([item (tell (tell NSMenuItem alloc) 
                                   initWithTitle: #:type _NSString ""
                                   action: #:type _SEL #f 
                                   keyEquivalent: #:type _NSString "")])
                   (tellv item setSubmenu: menu)
                   (tellv mb addItem: item)
                   (tellv item release)))])
  (let ([apple (tell (tell NSMenu alloc) initWithTitle: #:type _NSString "")])
    (let ([std (lambda (title sel [shortcut ""] [mods #f])
                 (let ([item (tell (tell NSMenuItem alloc) 
                                   initWithTitle: #:type _NSString title
                                   action: #:type _SEL sel
                                   keyEquivalent: #:type _NSString shortcut)])
                   (when mods
                     (tellv item setKeyEquivalentModifierMask: #:type _NSInteger mods))
                   (tellv item setTarget: app)
                   (tellv apple addItem: item)
                   (tellv item release)))])
      (std (format "About ~a" app-name) (selector orderFrontStandardAboutPanel:))
      (std "Preferences..."  #f)
      (tellv apple addItem: (tell NSMenuItem separatorItem))
      (let ([services (tell (tell NSMenu alloc) initWithTitle: #:type _NSString "Services")])
        (tellv app setServicesMenu: services)
        (let ([item (tell (tell NSMenuItem alloc) 
                          initWithTitle: #:type _NSString "Services"
                          action: #:type _SEL #f
                          keyEquivalent: #:type _NSString "")])
          (tellv item setSubmenu: services)
          (tellv apple addItem: item)
          (tellv item release)))
      (tellv apple addItem: (tell NSMenuItem separatorItem))
      (std (format "Hide ~a" app-name) (selector hide:) "h")
      (std "Hide Others" (selector hideOtherApplications:) "h" (bitwise-ior
                                                                NSAlternateKeyMask
                                                                NSCommandKeyMask))
      (std "Show All" (selector unhideAllApplications:))
      (tellv apple addItem: (tell NSMenuItem separatorItem))
      (std (format "Quit ~a" app-name) (selector terminate:) "q"))
    (add-one cocoa-mb apple)
    (tellv app setAppleMenu: apple)
    (tellv apple release)
    (tellv app setMainMenu: cocoa-mb)))

(defclass menu-bar% object%
  (define menus null)

  (def/public-unimplemented set-label-top)
  (def/public-unimplemented number)
  (def/public-unimplemented enable-top)

  (define/public (delete which pos)
    (set! menus (let loop ([menus menus]
                           [pos pos])
                  (cond
                   [(null? menus) menus]
                   [(zero? pos) (cdr menus)]
                   [else (cons (car menus)
                               (loop (cdr menus)
                                     pos))]))))

  (public [append-menu append])
  (define (append-menu menu title)
    (set! menus (append menus (list (cons menu title)))))

  (define/public (install)
    (let loop ()
      (when ((tell #:type _NSInteger cocoa-mb numberOfItems) . > . 1)
        (tellv cocoa-mb removeItem: (tell cocoa-mb itemAtIndex: #:type _NSInteger 1))
        (loop)))
    (for-each (lambda (menu)
                (send (car menu) install cocoa-mb (cdr menu)))
              menus))

  (super-new))
