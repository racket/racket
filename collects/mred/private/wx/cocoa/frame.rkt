#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
         "pool.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "queue.rkt"
         "menu-bar.rkt"
         "../../syntax.rkt"
         "../common/queue.rkt"
         "../../lock.rkt")
(unsafe!)
(objc-unsafe!)

(provide frame%)

;; ----------------------------------------

(import-class NSWindow NSGraphicsContext NSMenu NSPanel
              NSApplication NSAutoreleasePool)

(define front #f)

(define empty-mb (new menu-bar%))
(define root-fake-frame #f)

(define-objc-mixin (MyWindowMethods Superclass)
  [wx]
  [-a _scheme (getEventspace)
      (send wx get-eventspace)]
  [-a _BOOL (canBecomeKeyWindow) #t]
  [-a _BOOL (canBecomeMainWindow) #t]
  [-a _BOOL (windowShouldClose: [_id win])
      (queue-window-event wx (lambda ()
                               (when (send wx on-close)
                                 (send wx direct-show #f))))
      #f]
  [-a _void (windowDidResize: [_id notification])
      (when wx
        (queue-window-event wx (lambda ()
                                 (send wx on-size 0 0)
                                 (send wx clean-up))))]
  [-a _void (windowDidMove: [_id notification])
      (when wx
        (queue-window-event wx (lambda ()
                                 (send wx on-size 0 0))))]
  [-a _void (windowDidBecomeMain: [_id notification])
      (when wx
        (set! front wx)
        (send wx install-mb)
        (queue-window-event wx (lambda ()
                                 (send wx on-activate #t))))]
  [-a _void (windowDidResignMain: [_id notification])
      (when wx
        (when (eq? front wx) (set! front #f))
        (send empty-mb install)
        (queue-window-event wx (lambda ()
                                 (send wx on-activate #f))))])

(define-objc-class MyWindow NSWindow
  #:mixins (FocusResponder KeyMouseResponder MyWindowMethods)
  [wx])

(define-objc-class MyPanel NSPanel
  #:mixins (FocusResponder KeyMouseResponder MyWindowMethods)
  [wx])

(set-front-hook! (lambda () (values front
                                    (and front (send front get-eventspace)))))

(set-eventspace-hook! (lambda (w)
                        (or (and w
                                 (if (objc-is-a? w MyWindow)
                                     (tell #:type _scheme w getEventspace)
                                     #f))
                            (and front
                                 (send front get-eventspace)))))

(define (init-pos x y)
  (if (and (= x -11111)
           (= y -11111))
      (values 0 0)
      (values x y)))

(define frame%
  (class window%
    (init parent
          label
          x y w h
          style)
    (init [is-dialog? #f])

    (inherit get-cocoa get-parent
             pre-on-char pre-on-event)

    (super-new [parent parent]
               [cocoa
                (let ([is-sheet? (and #f
                                      is-dialog? 
                                      parent
                                      (not (send parent frame-is-dialog?)))])
                  (as-objc-allocation
                   (tell (tell (if is-sheet?
                                   MyPanel
                                   MyWindow)
                               alloc)
                         initWithContentRect: #:type _NSRect (let-values ([(x y) (init-pos x y)])
                                                               (make-NSRect (make-NSPoint x y)
                                                                            (make-NSSize (max 30 w) 
                                                                                         (max 0 h))))
                         styleMask: #:type _int (if (memq 'no-caption style)
                                                    NSBorderlessWindowMask
                                                    (bitwise-ior 
                                                     NSTitledWindowMask
                                                     (if is-sheet? NSUtilityWindowMask 0)
                                                     (if is-dialog?
                                                         0
                                                         (bitwise-ior 
                                                          NSClosableWindowMask
                                                          NSMiniaturizableWindowMask
                                                          (if (memq 'no-resize-border style)
                                                              0
                                                              NSResizableWindowMask)))))
                         backing: #:type _int NSBackingStoreBuffered
                         defer: #:type _BOOL NO)))]
               [no-show? #t])
    (define cocoa (get-cocoa))
    (tellv cocoa setDelegate: cocoa)

    (tellv cocoa setAcceptsMouseMovedEvents: #:type _BOOL #t)

    (define/override (get-cocoa-content) 
      (tell cocoa contentView))
    (define/override (get-cocoa-window) cocoa)
    (define/override (get-wx-window) this)

    (define/override (make-graphics-context)
      (as-objc-allocation
       (tell NSGraphicsContext graphicsContextWithWindow: cocoa)))

    (define is-a-dialog? is-dialog?)
    (define/public (frame-is-dialog?) is-a-dialog?)

    (define/public (clean-up)
      ;; When a window is resized, then any drawing that is in flight
      ;; might draw outside the canvas boundaries. Just refresh everything.
      (tellv cocoa display))

    (when label
      (tellv cocoa setTitle: #:type _NSString label))
    
    (define child-sheet #f)
    (define/public (get-sheet) child-sheet)
    (define/public (set-sheet s) (set! child-sheet s))

    (define/public (direct-show on?)
      (as-entry
       (lambda ()
         (when (and (not on?)
                    (eq? front this))
           (set! front #f)
           (send empty-mb install))
         (if on?
             (if (and is-a-dialog?
                      (let ([p (get-parent)])
                        (and p 
                             (not (send p get-sheet)))))
                 (let ([p (get-parent)])
                   (send p set-sheet this)
                   (tell (tell NSApplication sharedApplication)
                         beginSheet: cocoa
                         modalForWindow: (send p get-cocoa)
                         modalDelegate: #f
                         didEndSelector: #:type _SEL #f
                         contextInfo: #f))
                 (tellv cocoa makeKeyAndOrderFront: #f))
             (begin
               (when is-a-dialog?
                 (let ([p (get-parent)])
                   (when (and p
                              (eq? this (send p get-sheet)))
                     (send p set-sheet #f))))
               (tellv cocoa orderOut: #f)
               (let ([next
                      (let* ([pool (tell (tell NSAutoreleasePool alloc) init)]
                             [wins (tell (tell NSApplication sharedApplication) orderedWindows)])
                        (begin0
                         (for/or ([i (in-range (tell #:type _NSUInteger wins count))])
                           (let ([win (tell wins objectAtIndex: #:type _NSUInteger i)])
                             (and (tell #:type _BOOL win isVisible)
                                  win)))
                         (tellv pool release)))])
                 (cond
                  [next (tellv next makeKeyWindow)]
                  [root-fake-frame (send root-fake-frame install-mb)]
                  [else (void)]))))
         (register-frame-shown this on?))))

    (define/override (show on?)
      (direct-show on?))

    (define/override (is-shown?)
      (tell #:type _bool cocoa isVisible))

    (define/override (is-shown-to-root?)
      (is-shown?))

    (define/override (is-parent-enabled-to-root?)
      #t)

    (define/override (is-view?) #f)

    (define/public (flip-screen y)
      (let ([f (tell #:type _NSRect (tell cocoa screen) frame)])
        (- (NSSize-height (NSRect-size f)) y)))

    (define/override (flip y h) (flip-screen (+ y h)))

    (define/override (set-size x y w h)
      (unless (and (= x -1) (= y -1))
        (move x y))
      (let ([f (tell #:type _NSRect cocoa frame)])
        (tellv cocoa setFrame: 
               #:type _NSRect (make-NSRect 
                               (make-NSPoint (if (and is-a-dialog?
                                                      (let ([p (get-parent)])
                                                        (and p
                                                             (eq? this (send p get-sheet)))))
                                                 ;; need to re-center sheet:
                                                 (let* ([p (get-parent)]
                                                        [px (send p get-x)]
                                                        [pw (send p get-width)])
                                                   (+ px (/ (- pw w) 2)))
                                                 ;; keep current x position:
                                                 (NSPoint-x (NSRect-origin f)))
                                             (- (NSPoint-y (NSRect-origin f))
                                                (- h
                                                   (NSSize-height (NSRect-size f)))))
                               (make-NSSize w h))
               display: #:type _BOOL #t)))
    (define/override (move x y)
      (tellv cocoa setFrameTopLeftPoint: #:type _NSPoint (make-NSPoint x (flip-screen y))))

    (define/override (center dir wrt)
      (let ([f (tell #:type _NSRect cocoa frame)]
            [s (tell #:type _NSRect (tell cocoa screen) frame)])
        (tellv cocoa setFrame: 
               #:type _NSRect (make-NSRect (make-NSPoint 
                                            (if (or (eq? dir 'both)
                                                    (eq? dir 'horizontal))
                                                (/ (- (NSSize-width (NSRect-size s))
                                                      (NSSize-width (NSRect-size f)))
                                                   2)
                                                (NSPoint-x (NSRect-origin f)))
                                            (if (or (eq? dir 'both)
                                                    (eq? dir 'vertical))
                                                (/ (- (NSSize-height (NSRect-size s))
                                                      (NSSize-height (NSRect-size f)))
                                                   2)
                                                (NSPoint-x (NSRect-origin f))))
                                           (NSRect-size f))
               display: #:type _BOOL #t)))

    (define/public (enforce-size min-x min-y max-x max-y inc-x inc-y)
      (define (adj v) (if (negative? v) 32000 v))
      (tellv cocoa setMinSize: #:type _NSSize (make-NSSize (max min-x 1)
                                                           (max min-y 1)))
      (tellv cocoa setMaxSize: #:type _NSSize (make-NSSize (adj max-x)
                                                           (adj max-y)))
      (tellv cocoa setResizeIncrements: #:type _NSSize (make-NSSize inc-x inc-y)))

    (define hide-mb? (and (memq 'hide-menu-bar style) #t))
    (define mb #f)
    (define/public (get-menu-bar) mb)
    (define/public (set-menu-bar _mb) 
      (set! mb _mb)
      (send mb set-top-window this)
      (when (tell #:type _BOOL cocoa isMainWindow)
        (install-mb)))

    (define/public (install-mb)
      (tellv NSMenu setMenuBarVisible: #:type _BOOL (not hide-mb?))
      (if mb
          (send mb install)
          (send empty-mb install)))

    (define/public (on-activate on?) (void))

    (define/public (set-icon bm1 bm2 mode) (void)) ;; FIXME

    (define/override (call-pre-on-event w e)
      (pre-on-event w e))
    (define/override (call-pre-on-char w e)
      (pre-on-char w e))

    (define/public (on-menu-click) (void))

    (def/public-unimplemented on-toolbar-click)
    (def/public-unimplemented on-menu-command)
    (def/public-unimplemented on-mdi-activate)
    (def/public-unimplemented on-close)
    (define/public (designate-root-frame)
      (set! root-fake-frame this))
    (def/public-unimplemented system-menu)

    (define/public (set-modified on?)
      ;; Use standardWindowButton: ...
      (void))

    (define/public (create-status-line) (void))
    (define/public (set-status-text s) (void))
    (def/public-unimplemented status-line-exists?)

    (define/public (is-maximized?)
      (tell #:type _BOOL cocoa isZoomed))
    (define/public (maximize on?)
      (unless (eq? (tell #:type _BOOL cocoa isZoomed)
                   (and on? #t))
        (tellv cocoa zoom: cocoa)))

    (def/public-unimplemented iconized?)
    (def/public-unimplemented iconize)

    (define/public (set-title s)
      (tellv cocoa setTitle: #:type _NSString s))))
