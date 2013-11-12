#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
         "pool.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "queue.rkt"
         "menu-bar.rkt"
         "cursor.rkt"
         "../../syntax.rkt"
         "../common/queue.rkt"
         "../common/freeze.rkt"
         "../../lock.rkt")

(provide 
 (protect-out frame%
              location->window
              get-front))

;; ----------------------------------------

(import-class NSWindow NSGraphicsContext NSMenu NSPanel
              NSApplication NSAutoreleasePool NSScreen
              NSToolbar)

(define NSWindowCloseButton 0)
(define NSWindowToolbarButton 3)

(define front #f)

(define (get-front) front)

(define empty-mb (new menu-bar%))
(define root-fake-frame #f)

;; Maps window numbers to weak boxes of frame objects;
;;  the weak-box layer is needed to avoid GC-accounting
;;  problems.
(define all-windows (make-hash))

;; called in atomic mode
(define (send-screen-change-notifications flags)
  (when (zero? (bitwise-and flags 1)) ;; discard the "about to change" notifications
    (for ([b (in-hash-values all-windows)])
      (define f (weak-box-value b))
      (when f
        (define e (send f get-eventspace))
        (unless (eventspace-shutdown? e)
          (parameterize ([current-eventspace e])
            (queue-callback
             (λ ()
               (send f display-changed)))))))))

(set-screen-changed-callback! send-screen-change-notifications)

(define-objc-mixin (RacketWindowMethods Superclass)
  [wxb]
  [-a _scheme (getEventspace)
      (let ([wx (->wx wxb)])
        (and wx (send wx get-eventspace)))]
  [-a _BOOL (canBecomeKeyWindow)
      (let ([wx (->wx wxb)])
        (and wx
             (not (other-modal? wx))))]
  [-a _BOOL (canBecomeMainWindow)
      (let ([wx (->wx wxb)])
        (or (not wx)
            (not (send wx floating?))))]
  [-a _BOOL (windowShouldClose: [_id win])
      (queue-window*-event wxb (lambda (wx)
                                 (unless (other-modal? wx)
                                   (when (send wx on-close)
                                     (atomically
                                      (send wx direct-show #f))))))
      #f]
  [-a _void (windowDidResize: [_id notification])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (queue-window-event wx (lambda ()
                                     (send wx queue-on-size)
                                     (send wx clean-up)))
            ;; Live resize:
            (constrained-reply (send wx get-eventspace)
                               (lambda () 
                                 (pre-event-sync #t)
                                 (let loop () (when (yield/no-sync) (loop))))
                               (void)))))]
  [-a _void (windowDidMove: [_id notification])
      (when wxb
        (queue-window*-event wxb (lambda (wx)
                                   (send wx queue-on-size))))]
  [-a _void (windowDidBecomeMain: [_id notification])
      ;; We check whether the window is visible because
      ;; clicking the dock item tries to resurrect a hidden
      ;; frame. See also `setOneShot' below.
      (when (tell #:type _BOOL self isVisible)
        (when wxb
          (let ([wx (->wx wxb)])
            (when wx
              ;; Sometimes, a sheet becomes the main window and the parent
              ;; still thinks that the parent is the main window. Tell
              ;; the parent otherwise.
              (let ([p (send wx get-parent)])
                (when p
                  (let ([s (send p get-sheet)])
                    (when (eq? s wx)
                      (let ([parent (send p get-cocoa)])
                        (when (tell #:type _BOOL parent isMainWindow)
                          ;; The Cocoa docs say never to call this method directly,
                          ;; but we're trying to fix up a case where Cocoa seems
                          ;; to be confused:
                          (tellv parent resignMainWindow)))))))
              (set! front wx)
              (send wx install-wait-cursor)
              (send wx install-mb)
              (queue-window-event wx (lambda ()
                                       (send wx on-activate #t)))))))]
  [-a _void (windowDidBecomeKey: [_id notification])
      (when (tell #:type _BOOL self isVisible)
        (when wxb
          (let ([wx (->wx wxb)])
            (when wx
              (send wx notify-responder #t)))))]
  [-a _void (windowDidResignMain: [_id notification])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (when (eq? front wx) 
              (set! front #f)
              (send wx uninstall-wait-cursor))
            (if root-fake-frame
                (send root-fake-frame install-mb)
                (send empty-mb install))
            (queue-window-event wx (lambda ()
                                     (send wx on-activate #f))))))]
  [-a _void (windowDidResignKey: [_id notification])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (send wx notify-responder #f))))]
  [-a _void (windowDidMiniaturize: [_id notification])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (send wx force-window-focus))))]
  [-a _void (windowDidEndSheet: [_id notification])
      ;; In some cases, the window that has a sheet
      ;; stays main even as its sheet becomes main, so
      ;; we need to make the containing window become main
      ;; when the sheet goes away.
      (when (equal? self (tell app mainWindow))
        (tell self windowDidBecomeMain: notification))]
  [-a _void (toggleToolbarShown: [_id sender])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (queue-window-event wx
                                (lambda () (send wx on-toolbar-click))))))
      (void)])

(define-objc-class RacketWindow NSWindow
  #:mixins (FocusResponder KeyMouseResponder RacketWindowMethods)
  [wxb])

(define-objc-class RacketPanel NSPanel
  #:mixins (FocusResponder KeyMouseResponder RacketWindowMethods)
  [wxb])

(set-front-hook! (lambda () 
                   (let ([f (or front
                                root-fake-frame)])
                     (values f
                             (and f (send f get-eventspace))))))

(set-eventspace-hook! (lambda (evt w)
                        (define (is-mouse-or-key?)
                          (bitwise-bit-set? MouseAndKeyEventMask
                                            (tell #:type _NSInteger evt type)))
                        (cond
                         [w
                          (and (or (not root-fake-frame)
                                   ;; only mouse and key events in the root
                                   ;; frame need to be dispatched in the root
                                   ;; eventspace:
                                   (not (ptr-equal? w (send root-fake-frame get-cocoa)))
                                   (is-mouse-or-key?))
                               (or (objc-is-a? w RacketWindow)
                                   (objc-is-a? w RacketPanel))
                               (tell #:type _scheme w getEventspace))]
                         [front (send front get-eventspace)]
                         [root-fake-frame 
                          (and (is-mouse-or-key?)
                               (send root-fake-frame get-eventspace))]
                         [else #f])))

(define frame%
  (class window%
    (init parent
          label
          x y w h
          style)
    (init [is-dialog? #f])

    (inherit get-cocoa get-parent
             get-eventspace
             pre-on-char pre-on-event
             get-x
             on-new-child
             is-window-enabled?)

    (super-new [parent parent]
               [cocoa
                (let ([is-sheet? (and #f
                                      is-dialog? 
                                      parent
                                      (not (send parent frame-is-dialog?)))]
                      [init-rect (make-NSRect (make-init-point x y)
                                              (make-NSSize (max 30 w) 
                                                           (max (if (memq 'no-caption style)
                                                                    1
                                                                    22)
                                                                h)))])
                  (let ([c (as-objc-allocation
                            (tell (tell (if (or is-sheet? (memq 'float style))
                                            RacketPanel
                                            RacketWindow)
                                        alloc)
                                  initWithContentRect: #:type _NSRect init-rect
                                  styleMask: #:type _int (if (memq 'no-caption style)
                                                             NSBorderlessWindowMask
                                                             (bitwise-ior 
                                                              NSTitledWindowMask
                                                              (if is-sheet? NSUtilityWindowMask 0)
                                                              (if is-dialog?
                                                                  (bitwise-ior
                                                                   (if (memq 'close-button style)
                                                                       NSClosableWindowMask
                                                                       0)
                                                                   (if (memq 'resize-border style)
                                                                       NSResizableWindowMask
                                                                       0))
                                                                  (bitwise-ior 
                                                                   NSClosableWindowMask
                                                                   NSMiniaturizableWindowMask
                                                                   (if (memq 'no-resize-border style)
                                                                       0
                                                                       NSResizableWindowMask)))))
                                  backing: #:type _int NSBackingStoreBuffered
                                  defer: #:type _BOOL NO))])
                    ;; use init rect as frame size, not content size
                    (tellv c setFrame: #:type _NSRect init-rect display: #:type _BOOL #f)
                    c))]
               [no-show? #t])
    (define cocoa (get-cocoa))
    (tellv cocoa setDelegate: cocoa)

    (when (memq 'toolbar-button style)
      (atomically
       (let ([tb (tell (tell NSToolbar alloc) initWithIdentifier: #:type _NSString "Ok")])
         (tellv cocoa setToolbar: tb)
         (tellv tb setVisible: #:type _BOOL #f)
         (tellv tb release))))

    (internal-move #f (or y 0))

    (tellv cocoa setAcceptsMouseMovedEvents: #:type _BOOL #t)

    ;; Setting the window in one-shot mode helps prevent the
    ;;  frame from being resurrected by a click on the dock icon.
    (tellv cocoa setOneShot: #:type _BOOL #t)

    (define/override (get-cocoa-content) 
      (tell cocoa contentView))
    (define/override (get-cocoa-window) cocoa)
    (define/override (get-wx-window) this)

    (define/override (make-graphics-context)
      (tell cocoa graphicsContext)
      #;
      (as-objc-allocation
       (tell NSGraphicsContext graphicsContextWithWindow: cocoa)))

    (define is-a-dialog? is-dialog?)
    (define/public (frame-is-dialog?) is-a-dialog?)

    (define not-sheet? (and (memq 'no-sheet style) #t))

    (define/public (frame-relative-dialog-status win) #f)
    (define/override (get-dialog-level) 0)

    (define/public (clean-up)
      ;; When a window is resized, then any drawing that is in flight
      ;; might draw outside the canvas boundaries. Just refresh everything.
      (tellv cocoa display))

    (when label
      (tellv cocoa setTitle: #:type _NSString label))
    
    (define child-sheet #f)
    (define/public (get-sheet) child-sheet)
    (define/public (set-sheet s) (set! child-sheet s))

    (define caption? (not (memq 'no-caption style)))
    (define float? (memq 'float style))
    (define/public (can-have-sheet?) caption?)
    (define/public (floating?) float?)

    (when float?
      (tell cocoa setFloatingPanel: #:type _BOOL #t))

    (define/public (direct-show on?)
      ;; in atomic mode
      (when (and (not on?)
                 (eq? front this))
        (set! front #f)
        (send empty-mb install))
      (if on?
          (show-children)
          (hide-children))
      (if on?
          (if (and is-a-dialog?
                   (not not-sheet?)
                   (let ([p (get-parent)])
                     (and p 
                          (send p can-have-sheet?)
                          (not (send p get-sheet)))))
              (let ([p (get-parent)])
                (send p set-sheet this)
                (tellv (tell NSApplication sharedApplication)
                       beginSheet: cocoa
                       modalForWindow: (send p get-cocoa)
                       modalDelegate: #f
                       didEndSelector: #:type _SEL #f
                       contextInfo: #f))
              (if float?
                  (tellv cocoa orderFront: #f)
                  (tellv cocoa makeKeyAndOrderFront: #f)))
          (begin
            (when is-a-dialog?
              (let ([p (get-parent)])
                (when (and p
                           (eq? this (send p get-sheet)))
                  (send p set-sheet #f)
                  (tell (tell NSApplication sharedApplication)
                        endSheet: cocoa))))
            (when (is-shown?) ; otherwise, `deminiaturize' can show the window
              (tellv cocoa deminiaturize: #f)
              (tellv cocoa orderOut: #f))
            (force-window-focus)))
      (register-frame-shown this on?)
      (let ([num (tell #:type _NSInteger cocoa windowNumber)])
        (if on?
            (hash-set! all-windows num (make-weak-box this))
            (hash-remove! all-windows num)))
      (when on?
        (let ([b (eventspace-wait-cursor-count (get-eventspace))])
          (set-wait-cursor-mode (not (zero? b))))))

    (define/override (show on?)
      (let ([es (get-eventspace)])
        (when on?
          (when (eventspace-shutdown? es)
            (error (string->symbol
                    (format "show method in ~a" (if is-a-dialog? 'dialog% 'frame%)))
                   "the eventspace hash been shutdown"))
          (when saved-child
            (if (eq? (current-thread) (eventspace-handler-thread es))
                (do-paint-children)
                (let ([s (make-semaphore)])
                  (queue-callback (lambda ()
                                    (do-paint-children)
                                    (semaphore-post s)))
                  (sync/timeout 1 s))))))
      (atomically
       (direct-show on?)))

    (define/public (force-window-focus)
      (let ([next (get-app-front-window)])
        (cond
         [next 
          (tellv next makeKeyWindow)]          
         [root-fake-frame 
          ;; Make key focus shift to root frame:
          (let ([root-cocoa (send root-fake-frame get-cocoa)])
            (tellv root-cocoa orderFront: #f)
            (tellv root-cocoa makeKeyWindow)
            (tellv root-cocoa orderOut: #f))
          ;; Install root frame's menu bar:
          (send root-fake-frame install-mb)]
         [else (void)])))

    (define/private (do-paint-children)
      (when saved-child
        (send saved-child paint-children))
      (yield-refresh)
      (try-to-sync-refresh))

    (define/public (destroy)
      (when child-sheet (send child-sheet destroy))
      (atomically
       (direct-show #f)))

    (define/override (hide-children)
      (when saved-child
        (send saved-child hide-children)))
    (define/override (show-children)
      (when saved-child
        (send saved-child show-children)))
    (define/override (fixup-locations-children)
      (when saved-child
        (send saved-child fixup-locations-children)))

    (define/override (children-accept-drag on?)
      (when saved-child
        (send saved-child child-accept-drag on?)))

    (define/override (enable-window on?)
      (when saved-child
        (send saved-child enable-window (and on? (is-window-enabled?)))))

    (define/override (is-shown?)
      (or (tell #:type _bool cocoa isVisible)
          (tell #:type _bool cocoa isMiniaturized)))

    (define/override (is-shown-to-root?)
      (is-shown?))

    (define/override (is-shown-to-before-root?) #t)

    (define/override (is-parent-enabled-to-root?)
      #t)

    (define/override (is-view?) #f)

    (define is-main? #f)
    (define first-responder #f)

    (define saved-child #f)
    (define/override (register-child child on?)
      (unless on? (error 'register-child-in-frame "did not expect #f"))
      (unless (or (not saved-child) (eq? child saved-child))
        (error 'register-child-in-frame "expected only one child"))
      (set! saved-child child)
      (on-new-child child #t))

    (define/override (refresh-all-children)
      (when saved-child
        (send saved-child refresh)))

    (define/override (set-cursor c)
      (when saved-child
        (send saved-child set-cursor c)))

    (define/public (notify-responder on?)
      (set! is-main? on?)
      (when first-responder
        (do-notify-responder first-responder on?)))

    (define/private (do-notify-responder wx on?)
      (send wx focus-is-on on?)
      (queue-window-event wx
                          (if on?
                              (lambda () (send wx on-set-focus))
                              (lambda () (send wx on-kill-focus)))))

    (define/override (is-responder wx on?)
      (unless (and (not on?)
                   (not (eq? first-responder wx)))
        (if on?
            (set! first-responder wx)
            (set! first-responder #f))
        (unless on?
          (tellv cocoa makeFirstResponder: #f))
        (when is-main?
          (do-notify-responder wx on?))))

    (define/public (get-focus-window [even-if-not-active? #f])
      (let ([f-cocoa (tell cocoa firstResponder)])
        (and f-cocoa
             (or even-if-not-active?
                 (tell #:type _BOOL cocoa isKeyWindow))
             (->wx (get-ivar f-cocoa wxb)))))

    (define/public (install-wait-cursor)
      (when (positive? (eventspace-wait-cursor-count (get-eventspace)))
        (tellv (get-wait-cursor-handle) set)))

    (define/public (uninstall-wait-cursor)
      (when (positive? (eventspace-wait-cursor-count (get-eventspace)))
        (tellv arrow-cursor-handle set)))

    (define/public (set-wait-cursor-mode on?)
      (if on?
          (tell cocoa disableCursorRects)
          (tell cocoa enableCursorRects))
      (when (eq? this front)
        (if on?
            (install-wait-cursor)
            (uninstall-wait-cursor))))

    (define/override (start-no-cursor-rects)
      (tell cocoa disableCursorRects))

    (define/override (end-no-cursor-rects)
      (unless (positive? (eventspace-wait-cursor-count (get-eventspace)))
        (tell cocoa enableCursorRects)))

    (define/public (flip-screen y)
      (let ([f (tell #:type _NSRect (tell NSScreen mainScreen) frame)])
        (- (NSSize-height (NSRect-size f)) y)))

    (define/override (flip y h) (flip-screen (+ y h)))

    (define/override (get-y)
      (- (super get-y) (get-menu-bar-height)))

    (define/override (set-size x y w h)
      (unless (and (equal? x -1) (equal? y -1))
        (internal-move x y))
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
                                             ;; keep current y position:
                                             (- (NSPoint-y (NSRect-origin f))
                                                (- h
                                                   (NSSize-height (NSRect-size f)))))
                               (make-NSSize w h))
               display: #:type _BOOL #t)))
    (define/override (internal-move x y)
      (let ([x (if (not x) (get-x) x)]
            [y (if (not y) (get-y) y)])
        (tellv cocoa setFrameTopLeftPoint: #:type _NSPoint (make-NSPoint x (- (flip-screen y)
                                                                              (get-menu-bar-height))))))

    (define/override (center dir wrt)
      (let ([f (tell #:type _NSRect cocoa frame)]
            [w (if wrt
                   (tell #:type _NSRect (send wrt get-cocoa) frame)
                   (tell #:type _NSRect (tell cocoa screen) frame))])
        (tellv cocoa setFrame: 
               #:type _NSRect (make-NSRect (make-NSPoint 
                                            (if (or (eq? dir 'both)
                                                    (eq? dir 'horizontal))
                                                (+ (quotient (- (NSSize-width (NSRect-size w))
                                                                (NSSize-width (NSRect-size f)))
                                                             2)
                                                   (NSPoint-x (NSRect-origin w)))
                                                (NSPoint-x (NSRect-origin f)))
                                            (if (or (eq? dir 'both)
                                                    (eq? dir 'vertical))
                                                (+ (quotient (- (NSSize-height (NSRect-size w))
                                                                (NSSize-height (NSRect-size f)))
                                                             2)
                                                   (NSPoint-y (NSRect-origin w)))
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
      (when (or (tell #:type _BOOL cocoa isMainWindow)
                (and (eq? this root-fake-frame)
                     (not (get-app-front-window))))
        (install-mb)))

    (define/public (install-mb)
      (tellv NSMenu setMenuBarVisible: #:type _BOOL (not hide-mb?))
      (if mb
          (send mb install)
          (send empty-mb install)))

    (define/public (on-activate on?) (void))

    (define/public (set-icon bm1 [bm2 #f] [mode 'both]) (void)) ;; FIXME

    (define default-buttons (make-hasheq))
    (define checking-default? #f)
    (define/public (add-possible-default button)
      (hash-set! default-buttons button #t)
      (queue-default-button-check))
    (define/public (remove-possible-default button)
      (hash-remove! default-buttons button)
      (queue-default-button-check))
    (define/public (queue-default-button-check)
      (when (atomically
             (if checking-default?
                 #f
                 (begin
                   (set! checking-default? #t)
                   #t)))
        (queue-window-event 
         this
         (lambda ()
           (set! checking-default? #f)
           (for/or ([button (in-hash-keys default-buttons)])
             (send button be-default))))))

    (define/override (call-pre-on-event w e)
      (pre-on-event w e))
    (define/override (call-pre-on-char w e)
      (pre-on-char w e))

    (define/public (on-menu-click) (void))

    (define/public (on-toolbar-click) (void))
    (define/public (on-menu-command c) (void))
    (def/public-unimplemented on-mdi-activate)
    (define/public (on-close) #t)
    (define/public (designate-root-frame)
      (set! root-fake-frame this)
      ;; The first window shown is somehow sticky, so that it becomes
      ;; the main window if no windows are shown:
      (tellv cocoa orderFront: #f)
      (tellv cocoa orderOut: #f)
      (sync-cocoa-events))
    (def/public-unimplemented system-menu)

    (define/public (set-modified on?)
      (let ([b (tell cocoa standardWindowButton: #:type _NSInteger NSWindowCloseButton)])
        (tellv b setDocumentEdited: #:type _BOOL on?)))
    
    (define/public (is-maximized?)
      (tell #:type _BOOL cocoa isZoomed))
    (define/public (maximize on?)
      (unless (eq? (tell #:type _BOOL cocoa isZoomed)
                   (and on? #t))
        (tellv cocoa zoom: cocoa)))

    (define/public (iconized?)
      (tell #:type _BOOL cocoa isMiniaturized))
    (define/public (iconize on?)
      (if on?
          (tellv cocoa miniaturize: cocoa)
          (tellv cocoa deminiaturize: cocoa)))

    (define/public (set-title s)
      (tellv cocoa setTitle: #:type _NSString s))


    (define color-callback void)
    (define/public (set-color-callback cb)
      (set! color-callback cb))
    (define/override (on-color-change)
      (queue-window-event this (lambda () (color-callback))))
    
    (define/public (display-changed) (void))))

;; ----------------------------------------

(define (get-app-front-window)
  (atomically
   (with-autorelease
    (let ([wins (tell (tell NSApplication sharedApplication) orderedWindows)])
      (begin0
       (for/or ([i (in-range (tell #:type _NSUInteger wins count))])
         (let ([win (tell wins objectAtIndex: #:type _NSUInteger i)])
           (and (tell #:type _BOOL win isVisible)
                (tell #:type _BOOL win canBecomeMainWindow)
                (not (tell win parentWindow))
                (or (not root-fake-frame)
                    (not (ptr-equal? win (send root-fake-frame get-cocoa))))
                win))))))))

(define (location->window x y)
  (let ([n (tell #:type _NSInteger NSWindow 
                 windowNumberAtPoint: #:type _NSPoint 
                 (let ([f (tell #:type _NSRect (tell NSScreen mainScreen) frame)])
                   (make-NSPoint x (- (NSSize-height (NSRect-size f)) y)))
                 belowWindowWithWindowNumber: #:type _NSInteger 0)])
    (atomically (let ([b (hash-ref all-windows n #f)])
                  (and b (weak-box-value b))))))

(set-fixup-window-locations!
 (lambda ()
   ;; in atomic mode
   (for ([b (in-hash-values all-windows)])
     (let ([f (weak-box-value b)])
       (when f
         (send f fixup-locations-children))))))

