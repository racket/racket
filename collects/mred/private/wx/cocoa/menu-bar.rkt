#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         (only-in racket/list take drop)
         "../../syntax.rkt"
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "queue.rkt")

(provide 
 (protect-out menu-bar%
              get-menu-bar-height))

(import-class NSApplication NSMenu NSMenuItem NSProcessInfo NSScreen)

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

(define the-apple-menu #f)
(define recurring-for-command (make-parameter #f))

(define-objc-class RacketBarMenu NSMenu
  []
  ;; Disable automatic handling of keyboard shortcuts, except for
  ;;  the Apple menu
  (-a _BOOL (performKeyEquivalent: [_id evt])
      (or (and the-apple-menu
               (tell #:type _BOOL the-apple-menu performKeyEquivalent: evt))
          ;; Explicity send the event to the keyWindow:
          (and
           ;; Don't go into an infinite loop:
           (not (recurring-for-command))
           ;; Don't handle Cmd-` for cycling through windows:
           ;; [Is this right for all locales?]
           (not (equal? "`" (tell #:type _NSString evt characters)))
           ;; Otherwise, try to dispatch to the first respnder:
           (let ([w (tell app keyWindow)])
             (and w
                  (let ([r (tell w firstResponder)])
                    (and r
                         (begin
                           (parameterize ([recurring-for-command #t])
                             (let ([evt-type (tell #:type _NSInteger evt type)])
                               (cond
                                [(= NSKeyDown evt-type)
                                 (tell r keyDown: evt)]
                                [(= NSKeyUp evt-type)
                                 (tell r keyUp: evt)])))
                           #t)))))))))

(define cocoa-mb (tell (tell RacketBarMenu alloc) init))
(define current-mb #f)

;; Used to detect mouse click on the menu bar:
(define in-menu-bar-range
  (let ([f (tell #:type _NSRect 
                 (tell (tell NSScreen screens) objectAtIndex: #:type _NSUInteger 0)
                 frame)])
    (let ([x (NSPoint-x (NSRect-origin f))]
          [w (NSSize-width (NSRect-size f))]
          [y (+ (NSPoint-y (NSRect-origin f))
                (NSSize-height (NSRect-size f)))])
    (lambda (p)
      (let ([h (tell #:type _CGFloat cocoa-mb menuBarHeight)])
        (and (<= x (NSPoint-x p) (+ x w))
             (<= (- y h) (NSPoint-y p) y)))))))

(set-menu-bar-hooks! in-menu-bar-range)

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
    (let ([std (lambda (title sel [shortcut ""] [mods #f] [delegate? #f])
                 (let ([item (tell (tell NSMenuItem alloc) 
                                   initWithTitle: #:type _NSString title
                                   action: #:type _SEL sel
                                   keyEquivalent: #:type _NSString shortcut)])
                   (when mods
                     (tellv item setKeyEquivalentModifierMask: #:type _NSInteger mods))
                   (tellv item setTarget: (if delegate?
                                              (tell app delegate)
                                              app))
                   (tellv apple addItem: item)
                   (tellv item release)))])
      (std (format "About ~a" app-name) (selector openAbout:) "" #f #t)
      (std "Preferences..."  (selector openPreferences:) "," #f #t)
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
    (tellv app setMainMenu: cocoa-mb)
    (set! the-apple-menu apple)))

(tellv cocoa-mb setAutoenablesItems: #:type _BOOL #f)

(defclass menu-bar% object%
  (define menus null)

  (define/public (enable-top pos on?)
    (set-box! (cddr (list-ref menus pos)) on?)
    (when (eq? current-mb this)
      (tellv (tell cocoa-mb itemAtIndex: #:type _NSInteger (add1 pos))
             setEnabled: #:type _BOOL on?)))

  (define/public (delete which pos)
    (atomically
     (when (eq? current-mb this)
       (tellv cocoa-mb removeItem: 
              (tell cocoa-mb itemAtIndex: #:type _NSInteger (add1 pos))))
     (set! menus (let loop ([menus menus]
                            [pos pos])
                   (cond
                    [(null? menus) menus]
                    [(zero? pos) (cdr menus)]
                    [else (cons (car menus)
                                (loop (cdr menus)
                                      (sub1 pos)))])))))

  (public [append-menu append])
  (define (append-menu menu title)
    (set! menus (append menus (list (list* menu title (box #t)))))
    (send menu set-parent this)
    (when (eq? current-mb this)
      (send menu install cocoa-mb title #t)))

  (define/public (install)
    (let loop ()
      (when ((tell #:type _NSInteger cocoa-mb numberOfItems) . > . 1)
        (tellv cocoa-mb removeItem: (tell cocoa-mb itemAtIndex: #:type _NSInteger 1))
        (loop)))
    (for-each (lambda (menu)
                (send (car menu) install cocoa-mb (cadr menu) (unbox (cddr menu))))
              menus)
    (set! current-mb this))

  (define top-wx #f)
  (define/public (set-top-window top)
    (set! top-wx top))
  (define/public (get-top-window)
    top-wx)

  (define/public (set-label-top pos str)
    (set! menus (append
                 (take menus pos)
                 (let ([i (list-ref menus pos)])
                   (list (cons (car i) (cons str (cddr i)))))
                 (drop menus (add1 pos))))
    (when (eq? current-mb this)
      (tellv (tell cocoa-mb itemAtIndex: #:type _NSInteger 1)
             setTitle: #:type _NSString (clean-menu-label str))))

  (define/public (do-on-menu-click)
    (let ([es (send top-wx get-eventspace)])
      (when es
        (queue-event es (lambda ()
                          (send top-wx on-menu-click))))))

  (super-new))

(define initial-menubar-height 
  (inexact->exact (floor (tell #:type _CGFloat cocoa-mb menuBarHeight))))

(define (get-menu-bar-height)
  initial-menubar-height)
