#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
         "queue.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "keycode.rkt"
         "pool.rkt"
         "cursor.rkt"
         "../common/local.rkt"
         "../../lock.rkt"
         "../common/event.rkt"
         "../common/queue.rkt"
         "../common/delay.rkt"
         "../../syntax.rkt"
         "../common/freeze.rkt")
(unsafe!)
(objc-unsafe!)

(provide window%

         FocusResponder
         KeyMouseResponder
         KeyMouseTextResponder
         CursorDisplayer

         queue-window-event
         queue-window-refresh-event
         queue-window*-event
         request-flush-delay
         cancel-flush-delay
         make-init-point

         special-control-key
         special-option-key)

(define-local-member-name flip-client)

;; ----------------------------------------

(define special-control-key? #f)
(define special-control-key
  (case-lambda
   [() special-control-key?]
   [(on?) (set! special-control-key? (and on? #t))]))

(define special-option-key? #f)
(define special-option-key
  (case-lambda
   [() special-option-key?]
   [(on?) (set! special-option-key? (and on? #t))]))

;; ----------------------------------------

(define-objc-mixin (FocusResponder Superclass)
  [wxb]
  [-a _BOOL (acceptsFirstResponder)
      #t]
  [-a _BOOL (becomeFirstResponder)
      (and (super-tell becomeFirstResponder)
           (let ([wx (->wx wxb)])
             (when wx (send wx is-responder wx #t))
             #t))]
  [-a _BOOL (resignFirstResponder)
      (and (super-tell resignFirstResponder)
           (let ([wx (->wx wxb)])
             (when wx (send wx is-responder wx #f))
             #t))])

(import-class NSArray)
(import-protocol NSTextInput)

(define current-insert-text (make-parameter #f))

(define-objc-mixin (KeyMouseResponder Superclass)
  [wxb]
  [-a _void (mouseDown: [_id event]) 
      (unless (do-mouse-event wxb event 'left-down #t #f #f 'right-down)
        (super-tell #:type _void mouseDown: event))]
  [-a _void (mouseUp: [_id event]) 
      (unless (do-mouse-event wxb event 'left-up #f #f #f 'right-up)
        (super-tell #:type _void mouseUp: event))]
  [-a _void (mouseDragged: [_id event]) 
      (unless (do-mouse-event wxb event 'motion #t #f #f)
        (super-tell #:type _void mouseDragged: event))]
  [-a _void (mouseMoved: [_id event]) 
      ;; This event is sent to the first responder, instead of the
      ;; view under the mouse.
      (let* ([win (tell event window)]
             [view (and win (tell win contentView))]
             [hit (and view (tell view hitTest: #:type _NSPoint 
                                  (tell #:type _NSPoint event locationInWindow)))])
        (let loop ([hit hit])
          (when hit
            (if (tell #:type _BOOL hit respondsToSelector: #:type _SEL (selector doMouseMoved:))
                (unless (tell #:type _BOOL hit doMouseMoved: event)
                  (super-tell #:type _void mouseMoved: event))
                (loop (tell hit superview))))))]
  [-a _BOOL (doMouseMoved: [_id event]) 
      ;; called by mouseMoved:
      (and 
       ;; Make sure we're in the right eventspace:
       (let ([wx (->wx wxb)])
         (and wx
              (eq? (current-eventspace)
                   (send wx get-eventspace))))
       ;; Right event space, so handle the event:
       (do-mouse-event wxb event 'motion #f #f #f))]
  [-a _void (mouseEntered: [_id event]) 
      (unless (do-mouse-event wxb event 'enter 'check 'check 'check)
        (super-tell #:type _void mouseEntered: event))]
  [-a _void (mouseExited: [_id event]) 
      (unless (do-mouse-event wxb event 'leave 'check 'check 'check)
        (super-tell #:type _void mouseExited: event))]
  [-a _void (rightMouseDown: [_id event]) 
      (unless (do-mouse-event wxb event 'right-down #f #f #t)
        (super-tell #:type _void rightMouseDown: event))]
  [-a _void (rightMouseUp: [_id event]) 
      (unless (do-mouse-event wxb event 'right-up #f #f #f)
        (super-tell #:type _void rightMouseUp: event))]
  [-a _void (rightMouseDragged: [_id event]) 
      (unless (do-mouse-event wxb event 'motion #f #f #t)
        (super-tell #:type _void rightMouseDragged: event))]
  [-a _void (otherMouseDown: [_id event]) 
      (unless (do-mouse-event wxb event 'middle-down #f #t #f)
        (super-tell #:type _void otherMouseDown: event))]
  [-a _void (otherMouseUp: [_id event]) 
      (unless (do-mouse-event wxb event 'middle-up #f #f #f)
        (super-tell #:type _void otherMouseUp: event))]
  [-a _void (otherMouseDragged: [_id event]) 
      (unless (do-mouse-event wxb event 'motion #f #t #f)
        (super-tell #:type _void otherMouseDragged: event))]

  [-a _void (scrollWheel: [_id event])
      (unless (and (not (zero? (tell #:type _CGFloat event deltaY)))
                   (do-key-event wxb event self #f #t))
        (super-tell #:type _void scrollWheel: event))]
  
  [-a _void (keyDown: [_id event])
      (unless (do-key-event wxb event self #t #f)
        (super-tell #:type _void keyDown: event))]
  [-a _void (keyUp: [_id event])
      (unless (do-key-event wxb event self #f #f)
        (super-tell #:type _void keyUp: event))]
  [-a _void (insertText: [_NSString str])
      (let ([cit (current-insert-text)])
        (if cit
            (set-box! cit str)
            (let ([wx (->wx wxb)])
              (post-dummy-event) ;; to wake up in case of character palette insert 
              (when wx
                (queue-window-event wx (lambda ()
                                         (send wx key-event-as-string str)))))))]

  ;; for NSTextInput:
  [-a _BOOL (hasMarkedText) #f]
  [-a _id (validAttributesForMarkedText)
      (tell NSArray array)]
  [-a _void (unmarkText) (void)]
  [-a _NSRange (markedRange) (make-NSRange 0 0)]
  [-a _NSRange (selectedRange) (make-NSRange 0 0)]
  [-a _void (setMarkedText: [_id aString] selectedRange: [_NSRange selRange])
      (void)]
  [-a _id (validAttributesForMarkedText) #f]
  [-a _id (attributedSubstringFromRange: [_NSRange theRange]) #f]
  [-a _NSUInteger (characterIndexForPoint: [_NSPoint thePoint]) 0]
  [-a _NSInteger (conversationIdentifier) 0]
  [-a _void (doCommandBySelector: [_SEL aSelector]) (void)]
  [-a _NSRect (firstRectForCharacterRange: [_NSRange r]) (make-NSRect (make-NSPoint 0 0)
                                                                      (make-NSSize 0 0))])

(define-objc-mixin (KeyMouseTextResponder Superclass)
  #:mixins (KeyMouseResponder)
  #:protocols (NSTextInput)
  [wxb])

(define-objc-mixin (CursorDisplayer Superclass)
  [wxb]
  [-a _void (resetCursorRects)
      (let ([wx (->wx wxb)])
        (when wx
          (send wx reset-cursor-rects)))])

(define (do-key-event wxb event self down? wheel?)
  (let ([wx (->wx wxb)])
    (and
     wx
     (let ([inserted-text (box #f)])
       (unless wheel?
         ;; Calling `interpretKeyEvents:' allows key combinations to be
         ;; handled, such as option-e followed by e to produce é. The
         ;; call to `interpretKeyEvents:' typically calls `insertText:',
         ;; so we set `current-insert-text' to tell `insertText:' to just 
         ;; give us back the text in the parameter. For now, we ignore the
         ;; text and handle the event as usual, though probably we should
         ;; be doing something with it.
         (parameterize ([current-insert-text inserted-text])
           (tellv self interpretKeyEvents: (tell (tell NSArray alloc) 
                                                 initWithObjects: #:type (_ptr i _id) event
                                                 count: #:type _NSUInteger 1))))
       (let* ([modifiers (tell #:type _NSUInteger event modifierFlags)]
              [bit? (lambda (m b) (positive? (bitwise-and m b)))]
              [pos (tell #:type _NSPoint event locationInWindow)]
              [str (if wheel?
                       #f
                       (tell #:type _NSString event characters))]
              [control? (bit? modifiers NSControlKeyMask)]
              [option? (bit? modifiers NSAlternateKeyMask)]
              [delta-y (and wheel?
                            (tell #:type _CGFloat event deltaY))])
         (let-values ([(x y) (send wx window-point-to-view pos)])
           (let ([k (new key-event%
                         [key-code (if wheel?
                                       (if (positive? delta-y)
                                           'wheel-up
                                           'wheel-down)
                                       (or
                                        (map-key-code (tell #:type _ushort event keyCode))
                                        (if (string=? "" str)
                                            #\nul
                                            (let ([c (string-ref str 0)])
                                              (or (and control?
                                                       (char<=? #\u00 c #\u1a)
                                                       (let ([alt-str (tell #:type _NSString event charactersIgnoringModifiers)])
                                                         (and (string? alt-str)
                                                              (= 1 (string-length alt-str))
                                                              (string-ref alt-str 0))))
                                                  c)))))]
                         [shift-down (bit? modifiers NSShiftKeyMask)]
                         [control-down control?]
                         [meta-down (bit? modifiers NSCommandKeyMask)]
                         [alt-down option?]
                         [x (->long x)]
                         [y (->long y)]
                         [time-stamp (->long (* (tell #:type _double event timestamp) 1000.0))]
                         [caps-down (bit? modifiers NSAlphaShiftKeyMask)])])
             (unless wheel?
               (let ([alt-str (tell #:type _NSString event charactersIgnoringModifiers)])
                 (when (and (string? alt-str)
                            (= 1 (string-length alt-str)))
                   (let ([alt-code (string-ref alt-str 0)])
                     (unless (equal? alt-code (send k get-key-code))
                       (send k set-other-altgr-key-code alt-code)))))
               (when (and option? 
                          special-option-key?
                          (send k get-other-altgr-key-code))
                 ;; swap altenate with main
                 (let ([other (send k get-other-altgr-key-code)])
                   (send k set-other-altgr-key-code (send k get-key-code))
                   (send k set-key-code other)))
               (unless down?
                 ;; swap altenate with main
                 (send k set-key-release-code (send k get-key-code))
                 (send k set-key-code 'release)))
             (if (send wx definitely-wants-event? k)
                 (begin
                   (queue-window-event wx (lambda ()
                                            (send wx dispatch-on-char/sync k)))
                   #t)
                 (constrained-reply (send wx get-eventspace)
                                    (lambda () (send wx dispatch-on-char k #t))
                                    #t)))))))))

(define (do-mouse-event wxb event kind l? m? r? [ctl-kind kind])
  (let ([wx (->wx wxb)])
    (and
     wx
     (let* ([modifiers (tell #:type _NSUInteger event modifierFlags)]
            [bit? (lambda (m b) (positive? (bitwise-and m b)))]
            [pos (tell #:type _NSPoint event locationInWindow)])
       (let-values ([(x y) (send wx window-point-to-view pos)]
                    [(control-down) (bit? modifiers NSControlKeyMask)]
                    [(l?) (if (eq? l? 'check)
                              (send wx get-last-left-button)
                              l?)]
                    [(m?) (if (eq? m? 'check)
                              (send wx get-last-middle-button)
                              m?)]
                    [(r?) (if (eq? r? 'check)
                              (send wx get-last-right-button)
                              r?)])
         (let ([l? (and l? (not control-down))]
               [r? (or r? (and l? control-down))])
           (send wx set-last-buttons l? m? r?)
           (let ([m (new mouse-event%
                         [event-type (if control-down ctl-kind kind)]
                         [left-down l?]
                         [middle-down m?]
                         [right-down r?]
                         [x (->long x)]
                         [y (->long y)]
                         [shift-down (bit? modifiers NSShiftKeyMask)]
                         [meta-down (bit? modifiers NSCommandKeyMask)]
                         [alt-down (bit? modifiers NSAlternateKeyMask)]
                         [time-stamp (->long (* (tell #:type _double event timestamp) 1000.0))]
                         [caps-down (bit? modifiers NSAlphaShiftKeyMask)])])
             (cond
              [(send m dragging?) (void)]
              [(send m button-down?) 
               (send wx set-sticky-cursor)
               (send wx start-no-cursor-rects)]
              [(or l? m? r?) (void)]
              [else (send wx end-no-cursor-rects)])
             (if (send wx definitely-wants-event? m)
                 (begin
                   (queue-window-event wx (lambda ()
                                            (send wx dispatch-on-event/sync m)))
                   #t)
                 (constrained-reply (send wx get-eventspace)
                                    (lambda () (send wx dispatch-on-event m #t))
                                    #t)))))))))

(define window%
  (class object%
    (init-field parent 
                cocoa
                [no-show? #f])

    (super-new)

    (queue-autorelease-flush)

    (define eventspace (if parent
                           (send parent get-eventspace)
                           (current-eventspace)))

    (when (eventspace-shutdown? eventspace)
      (error '|GUI object initialization| "the eventspace has been shutdown"))

    (set-ivar! cocoa wxb (->wxb this))

    (unless no-show?
      (show #t)) 

    (define/public (focus-is-on on?)
      (void))
    
    (define is-responder? #f)

    (define/public (is-responder wx on?)
      (unless (eq? on? is-responder?)
        (set! is-responder? (and on? #t))
        (send parent is-responder wx on?)))

    (define/public (hide-children)
      (is-responder this #f)
      (focus-is-on #f))
    (define/public (show-children)
      (void))
    (define/public (fix-dc) 
      (void))
    (define/public (paint-children)
      (void))

    (define/public (get-cocoa) cocoa)
    (define/public (get-cocoa-content) cocoa)
    (define/public (get-cocoa-cursor-content) (get-cocoa-content))
    (define/public (get-cocoa-window) (send parent get-cocoa-window))
    (define/public (get-wx-window) (send parent get-wx-window))

    (define/public (get-dialog-level) 
      ;; called in event-pump thread
      (send parent get-dialog-level))

    (define/public (make-graphics-context)
      (and parent
           (send parent make-graphics-context)))

    (define/public (get-parent)
      parent)

    (define/public (get-eventspace) eventspace)

    (define is-on? #f)
    (define/public (show on?)
      (atomically
       (unless (eq? (and on? #t) is-on?)
         (if on?
             (tellv (send parent get-cocoa-content) addSubview: cocoa)
             (with-autorelease
              (tellv cocoa removeFromSuperview)))
         (set! is-on? (and on? #t))
         (maybe-register-as-child parent on?)
         (if on?
             (show-children)
             (begin
               (hide-children)
               (is-responder this #f))))))
    (define/public (maybe-register-as-child parent on?)
      ;; override this to call register-as-child if the window
      ;; can have the focus or otherwise needs show-state notifications.
      (void))
    (define/public (register-as-child parent on?)
      (send parent register-child this on?))
    (define/public (register-child child on?)
      (void))

    (define/public (is-shown?)
      (and (tell cocoa superview) #t))

    (define/public (is-shown-to-root?)
      (and (is-shown?)
           (send parent is-shown-to-root?)))

    (define/public (is-shown-to-before-root?)
      (and (is-shown?)
           (send parent is-shown-to-before-root?)))

    (define enabled? #t)
    (define/public (is-enabled-to-root?)
      (and (is-window-enabled?) (is-parent-enabled-to-root?)))
    (define/public (is-parent-enabled-to-root?)
      (send parent is-enabled-to-root?))
    (define/public (is-window-enabled?)
      enabled?)
    (define/public (enable on?)
      (set! enabled? on?))

    (define/private (get-frame)
      (let ([v (tell #:type _NSRect cocoa frame)])
        v))

    (define/public (flip y h)
      (if parent
          (let ([b (tell #:type _NSRect (send parent get-cocoa-content) bounds)])
            (- (NSSize-height (NSRect-size b)) (+ y h)))
          y))

    (define/public (flip-client y)
      (if (tell #:type _BOOL (get-cocoa-content) isFlipped)
          y
          (let ([r (tell #:type _NSRect (get-cocoa-content) bounds)])
            (- (NSSize-height (NSRect-size r)) 
               (- y (client-y-offset))))))
    (define/public (client-y-offset) 0)

    (define/public (is-view?) #t)
    (define/public (window-point-to-view pos)
      (let ([pos (if (is-view?)
                     (tell #:type _NSPoint (get-cocoa-content) 
                           convertPoint: #:type _NSPoint pos
                           fromView: #f)
                     pos)])
        (values (NSPoint-x pos)
                (flip-client (NSPoint-y pos)))))

    (define/public (get-x)
      (->long (NSPoint-x (NSRect-origin (get-frame)))))
    (define/public (get-y)
      (let ([r (get-frame)])
        (->long (flip (NSPoint-y (NSRect-origin r))
                      (NSSize-height (NSRect-size r))))))
    (define/public (get-width)
      (->long (NSSize-width (NSRect-size (get-frame)))))
    (define/public (get-height)
      (->long (NSSize-height (NSRect-size (get-frame)))))
    (define/public (get-position x y)
      (let* ([r (get-frame)]
             [p (NSRect-origin r)])
        (set-box! x (->long (NSPoint-x p)))
        (set-box! y (->long (flip (NSPoint-y p) (NSSize-height (NSRect-size r)))))))
    (define/public (get-size w h)
      (let ([s (NSRect-size (get-frame))])
        (set-box! w (->long (NSSize-width s)))
        (set-box! h (->long (NSSize-height s)))))

    (define/public (get-client-size w h)
      ;; May be called in Cocoa event-handling mode
      (let ([s (NSRect-size (tell #:type _NSRect (get-cocoa-content) bounds))])
        (set-box! w (->long (NSSize-width s)))
        (set-box! h (->long (NSSize-height s)))))

    (define/public (set-size x y w h)
      (let ([x (if (= x -11111) (get-x) x)]
            [y (if (= y -11111) (get-y) y)])
        (tellv cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint x (flip y h))
                                                           (make-NSSize w h)))))
    (define/public (move x y)
      (set-size x y (get-width) (get-height)))

    (define/public (drag-accept-files on?) 
      (void))

    (define/public (set-focus)
      (let ([w (tell cocoa window)])
        (when w
          (tellv w makeFirstResponder: (get-cocoa-content)))))
    (define/public (on-set-focus) (void))
    (define/public (on-kill-focus) (void))

    (define/public (definitely-wants-event? e) 
      ;; Called in Cocoa event-handling mode
      #f)

    (define/private (pre-event-refresh key?)
      ;; Since we break the connection between the
      ;; Cocoa queue and event handling, we
      ;; re-sync the display in case a stream of
      ;; events (e.g., key repeat) have a corresponding
      ;; stream of screen updates.
      (try-to-sync-refresh)
      (let ([cocoa-win (get-cocoa-window)])
        (when cocoa-win
          (tellv cocoa-win displayIfNeeded)
          (tellv cocoa-win flushWindowIfNeeded))))

    (define/public (dispatch-on-char/sync e)
      (pre-event-refresh #t)
      (dispatch-on-char e #f))
    (define/public (dispatch-on-char e just-pre?) 
      (cond
       [(other-modal? this) #t]
       [(call-pre-on-char this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-char e)) #t]))

    (define/public (dispatch-on-event/sync e)
      (pre-event-refresh #f)
      (dispatch-on-event e #f))
    (define/public (dispatch-on-event e just-pre?) 
      (cond
       [(other-modal? this) #t]
       [(call-pre-on-event this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-event e)) #t]))

    (define/public (call-pre-on-event w e)
      (or (send parent call-pre-on-event w e)
          (pre-on-event w e)))
    (define/public (call-pre-on-char w e)
      (or (send parent call-pre-on-char w e)
          (pre-on-char w e)))
    (define/public (pre-on-event w e) #f)
    (define/public (pre-on-char w e) #f)

    (define/public (key-event-as-string s)
      (dispatch-on-char (new key-event%
                             [key-code (string-ref s 0)]
                             [shift-down #f]
                             [control-down #f]
                             [meta-down #f]
                             [alt-down #f]
                             [x 0]
                             [y 0]
                             [time-stamp (current-milliseconds)] ; FIXME
                             [caps-down #f])
                        #f))

    (define/public (on-char s) (void))
    (define/public (on-event m) (void))
    (define/public (on-size x y) (void))

    (define last-l? #f)
    (define last-m? #f)
    (define last-r? #f)
    (define/public (set-last-buttons l? m? r?)
      (set! last-l? l?)
      (set! last-m? m?)
      (set! last-r? r?))
    (define/public (get-last-left-button) last-l?)
    (define/public (get-last-middle-button) last-m?)
    (define/public (get-last-right-button) last-r?)

    (define/public (set-sticky-cursor)
      (set! sticky-cursor? #t))

    (define/public (start-no-cursor-rects)
      (send (get-parent) start-no-cursor-rects))
    (define/public (end-no-cursor-rects)
      (set! sticky-cursor? #f)
      (send (get-parent) end-no-cursor-rects))

    (def/public-unimplemented on-drop-file)
    (def/public-unimplemented get-handle)
    (def/public-unimplemented set-phantom-size)

    (define/public (popup-menu m x y)
      (send m do-popup (get-cocoa-content) x (flip-client y)
            (lambda (thunk)
              (queue-window-event this thunk))))

    (define/public (center a b) (void))
    (def/public-unimplemented refresh)

    (define/public (screen-to-client xb yb)
      (let ([p (tell #:type _NSPoint (get-cocoa-content) 
                     convertPointFromBase: #:type _NSPoint
                     (tell #:type _NSPoint (get-cocoa-window)
                           convertScreenToBase:
                           #:type _NSPoint (make-NSPoint (unbox xb) 
                                                         (send (get-wx-window) flip-screen (unbox yb)))))])
        (set-box! xb (inexact->exact (floor (NSPoint-x p))))
        (set-box! yb (inexact->exact (floor (flip-client (NSPoint-y p)))))))

    (define/public (client-to-screen xb yb [flip-y? #t])
      (let* ([p (tell #:type _NSPoint (get-cocoa-window)
                      convertBaseToScreen:
                      #:type _NSPoint
                      (tell #:type _NSPoint (get-cocoa-content) 
                            convertPointToBase: #:type _NSPoint
                            (make-NSPoint (unbox xb) (flip-client (unbox yb)))))])
        (let ([new-y (if flip-y?
                         (send (get-wx-window) flip-screen (NSPoint-y p))
                         (NSPoint-y p))])
          (set-box! xb (inexact->exact (floor (NSPoint-x p))))
          (set-box! yb (inexact->exact (floor new-y))))))
      
    (def/public-unimplemented fit)

    (define cursor-handle #f)
    (define sticky-cursor? #f)
    (define/public (set-cursor c)
      (let ([h (if c
                   (send (send c get-driver) get-handle)
                   #f)])
        (unless (eq? h cursor-handle)
          (atomically
           (set! cursor-handle h)
           (when sticky-cursor? (tellv h set))
           (tellv (get-cocoa-window) invalidateCursorRectsForView: (get-cocoa-cursor-content))))))
    (define/public (reset-cursor-rects)
      ;; called in event-pump thread
      (when cursor-handle
        (let ([content (get-cocoa-cursor-content)])
          (let* ([r (tell #:type _NSRect content frame)]
                 [r (make-NSRect (make-NSPoint 0 0)
                                 (make-NSSize
                                  (- (NSSize-width (NSRect-size r))
                                     (get-cursor-width-delta))
                                  (NSSize-height (NSRect-size r))))])
            (tellv content addCursorRect: #:type _NSRect r cursor: cursor-handle)))))
    (define/public (get-cursor-width-delta) 0)
    
    (define/public (gets-focus?) #f)
    
    (def/public-unimplemented centre)))


;; ----------------------------------------

(define (queue-window-event wx thunk)
  (queue-event (send wx get-eventspace) thunk))

(define (queue-window-refresh-event wx thunk)
  (queue-refresh-event (send wx get-eventspace) thunk))

(define (queue-window*-event wxb proc)
  (let ([wx (->wx wxb)])
    (when wx
      (queue-event (send wx get-eventspace) (lambda () (proc wx))))))

(define (request-flush-delay cocoa-win)
  (do-request-flush-delay 
   cocoa-win
   (lambda (cocoa-win)
     (tellv cocoa-win disableFlushWindow))
   (lambda (cocoa-win)
     (tellv cocoa-win enableFlushWindow))))

(define (cancel-flush-delay req)
  (do-cancel-flush-delay 
   req
   (lambda (cocoa-win)
     (tellv cocoa-win enableFlushWindow))))

(define (make-init-point x y)
  (make-NSPoint (if (= x -11111)
                    0
                    x)
                (if (= y -11111)
                    0
                    y)))
