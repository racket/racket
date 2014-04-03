#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
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

(provide 
 (protect-out window%

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
              flush-display

              special-control-key
              special-option-key))

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
      (let ([wx (->wx wxb)])
        (or (not wx)
            (send wx can-be-responder?)))]
  [-a _BOOL (becomeFirstResponder)
      (and (super-tell becomeFirstResponder)
           (let ([wx (->wx wxb)])
             (when wx (send wx is-responder wx #t))
             #t))]
  [-a _BOOL (resignFirstResponder)
      (and (super-tell resignFirstResponder)
           (let ([wx (->wx wxb)])
             (when wx
               (send wx is-responder wx #f)
               (send wx set-saved-marked #f #f))
             #t))]
  [-a _void (changeColor: [_id sender])
      (let ([wx (->wx wxb)])
        (when wx (send wx on-color-change)))])

(import-class NSArray NSPanel NSTextView)
(import-protocol NSTextInput)

(define current-insert-text (make-parameter #f))
(define current-set-mark (make-parameter #f))

(define NSDragOperationCopy 1)

(import-class NSAttributedString)
(define _NSStringOrAttributed
  (make-ctype _id
              (lambda (v)
                (cast v _NSString _id))
              (lambda (v)
                (if (tell #:type _BOOL v isKindOfClass: (tell NSAttributedString class))
                    (tell #:type _NSString v string)
                    (cast v _id _NSString)))))

(define-objc-mixin (KeyMouseResponder Superclass)
  [wxb]
  [-a _void (mouseDown: [_id event]) 
      (unless (do-mouse-event wxb event 'left-down #t #f #f 'right-down)
        (super-tell #:type _void mouseDown: event)
        (let ([wx (->wx wxb)])
         (when wx
           (send wx post-mouse-down))))]
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
              (eq? (current-thread)
                   (eventspace-handler-thread
                    (send wx get-eventspace)))))
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
      (let ([delta-y (tell #:type _CGFloat event deltaY)]
            [delta-x (tell #:type _CGFloat event deltaX)])
        (let ([evts (append (cond
                             [(zero? delta-y) '()]
                             [(positive? delta-y) '(wheel-up)]
                             [else '(wheel-down)])
                            (cond
                             [(zero? delta-x) '()]
                             [(positive? delta-x) '(wheel-left)]
                             [else '(wheel-right)]))])
          (unless (and (pair? evts)
                       (do-key-event wxb event self #f #f evts))
            (super-tell #:type _void scrollWheel: event))))]
  
  [-a _void (keyDown: [_id event])
      (unless (do-key-event wxb event self #t #f #f)
        (super-tell #:type _void keyDown: event))]
  [-a _void (keyUp: [_id event])
      (unless (do-key-event wxb event self #f #f #f)
        (super-tell #:type _void keyUp: event))]
  [-a _void (flagsChanged: [_id event])
      (unless (do-key-event wxb event self #f #t #f)
        (super-tell #:type _void flagsChanged: event))]
  [-a _void (insertText: [_NSStringOrAttributed str])
      (set-saved-marked! wxb #f #f)
      (let ([cit (current-insert-text)])
        (if cit
            (set-box! cit str)
            (let ([wx (->wx wxb)])
              (post-dummy-event) ;; to wake up in case of character palette insert 
              (when wx
                (queue-window-event wx (lambda ()
                                         (send wx key-event-as-string str)))))))]

  ;; for NSTextInput:
  [-a _BOOL (hasMarkedText) (get-saved-marked wxb)]
  [-a _id (validAttributesForMarkedText)
      (tell NSArray array)]
  [-a _void (unmarkText) 
      (set-saved-marked! wxb #f #f)]
  [-a _NSRange (markedRange)
      (let ([saved-marked (get-saved-marked wxb)])
        (make-NSRange 0 (if saved-marked (string-length saved-marked) 0)))]
  [-a _NSRange (selectedRange) 
      (or (let ([s (get-saved-selected wxb)])
            (and s
                 (make-NSRange (car s) (cdr s))))
          (make-NSRange 0 0))]
  [-a _void (setMarkedText: [_NSStringOrAttributed aString] selectedRange: [_NSRange selRange])
      ;; We interpreter a call to `setMarkedText:' as meaning that the
      ;; key is a dead key for composing some other character.
      (let ([m (current-set-mark)]) (when m (set-box! m #t)))
      ;; At the same time, we need to remember the text:
      (set-saved-marked! wxb aString (cons (NSRange-location selRange) 
                                           (NSRange-length selRange)))
      (void)]
  [-a _id (validAttributesForMarkedText) #f]
  [-a _id (attributedSubstringFromRange: [_NSRange theRange])
      (let ([saved-marked (get-saved-marked wxb)])
        (and saved-marked
             (let ([s (tell (tell NSAttributedString alloc) 
                            initWithString: #:type _NSString
                            (range-substring saved-marked theRange))])
               (tellv s autorelease)
               s)))]

  [-a _NSUInteger (characterIndexForPoint: [_NSPoint thePoint]) 0]
  [-a _NSInteger (conversationIdentifier) 0]
  [-a _void (doCommandBySelector: [_SEL aSelector]) (void)]
  [-a _NSRect (firstRectForCharacterRange: [_NSRange r]) 
      ;; This location is used to place a window for multi-character
      ;; input, such as when typing Chinese with Pinyin
      (let ([f (tell #:type _NSRect self frame)]
            [pt (tell #:type _NSPoint (tell self window)
                      convertBaseToScreen:
                      #:type _NSPoint
                      (tell #:type _NSPoint self
                            convertPoint: #:type _NSPoint
                            (make-NSPoint 0 0)
                            toView: #f))])
        (make-NSRect pt (NSRect-size f)))]

  ;; Dragging:
  [-a _int (draggingEntered: [_id info])
      NSDragOperationCopy]
  [-a _BOOL (prepareForDragOperation: [_id info])
      #t]
  [-a _BOOL (performDragOperation: [_id info])
      (let ([wx (->wx wxb)])
        (when wx
          (with-autorelease
           (let ([pb (tell info draggingPasteboard)])
             (let ([data (tell pb propertyListForType: NSFilenamesPboardType)])
               (when data
                 (for ([i (in-range (tell #:type _NSUInteger data count))])
                   (let ([s (tell #:type _NSString data objectAtIndex: #:type _NSUInteger i)])
                     (queue-window-event wx 
                                         (lambda ()
                                           (send wx do-on-drop-file s)))))))))))
      #t])
(define (set-saved-marked! wxb str sel)
  (let ([wx (->wx wxb)])
    (when wx
      (send wx set-saved-marked str sel))))
(define (get-saved-marked wxb)
  (let ([wx (->wx wxb)])
    (and wx
         (send wx get-saved-marked))))
(define (get-saved-selected wxb)
  (let ([wx (->wx wxb)])
    (and wx
         (send wx get-saved-selected))))
(define (range-substring s range)
  (let ([start (min (max 0 (NSRange-location range)) (string-length s))])
    (substring s start (max (+ start (NSRange-length range)) 
                            (string-length s)))))

(define-objc-class InputMethodPanel NSPanel
  []
  [-a _BOOL (canBecomeKeyWindow) #f]
  [-a _BOOL (canBecomeMainWindow) #f]
  [-a _void (windowDidResize: [_id notification])
      (reset-input-method-window-size)])

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

(define (do-key-event wxb event self down? mod-change? wheel)
  (let ([wx (->wx wxb)])
    (and
     wx
     (let ([inserted-text (box #f)]
           [set-mark (box #f)]
           [had-saved-text? (and (send wx get-saved-marked) #t)])
       (when down?
         ;; Calling `interpretKeyEvents:' allows key combinations to be
         ;; handled, such as option-e followed by e to produce é. The
         ;; call to `interpretKeyEvents:' typically calls `insertText:',
         ;; so we set `current-insert-text' to tell `insertText:' to just 
         ;; give us back the text in the parameter. For now, we ignore the
         ;; text and handle the event as usual, though probably we should
         ;; be doing something with it.
         (parameterize ([current-insert-text inserted-text]
                        [current-set-mark set-mark])
           (let ([array (tell (tell NSArray alloc) 
                              initWithObjects: #:type (_ptr i _id) event
                              count: #:type _NSUInteger 1)])
             (tellv self interpretKeyEvents: array)
             (tellv array release))))
       (let* ([modifiers (tell #:type _NSUInteger event modifierFlags)]
              [bit? (lambda (m b) (positive? (bitwise-and m b)))]
              [pos (tell #:type _NSPoint event locationInWindow)]
              [str (cond
                    [wheel #f]
                    [mod-change? #f]
                    [(unbox set-mark) ""] ; => dead key for composing characters
                    [(unbox inserted-text)]
                    [else
                     (tell #:type _NSString event characters)])]
              [dead-key? (unbox set-mark)]
              [control? (bit? modifiers NSControlKeyMask)]
              [option? (bit? modifiers NSAlternateKeyMask)]
              [codes (cond
                      [wheel wheel]
                      [mod-change? (case (tell #:type _ushort event keyCode)
                                     [(56) '(shift)]
                                     [(59) '(control)]
                                     [(60) '(rshift)]
                                     [(62) '(rcontrol)]
                                     [else '()])]
                      [had-saved-text? str]
                      [(map-key-code (tell #:type _ushort event keyCode))
                       => list]
                      [(string=? "" str) '(#\nul)]
                      [(and (= 1 (string-length str))
                            (let ([c (string-ref str 0)])
                              (or (and control?
                                       (char<=? #\u00 c #\u1F)
                                       (let ([alt-str (tell #:type _NSString event charactersIgnoringModifiers)])
                                         (and (string? alt-str)
                                              (= 1 (string-length alt-str))
                                              (string-ref alt-str 0)))))))
                       => list]
                      [else str])])
         (for/fold ([result dead-key?]) ([one-code codes])
           (or
            ;; Handle one key event
            (let-values ([(x y) (send wx window-point-to-view pos)])
              (let ([k (new key-event%
                            [key-code one-code]
                            [shift-down (bit? modifiers NSShiftKeyMask)]
                            [control-down control?]
                            [meta-down (bit? modifiers NSCommandKeyMask)]
                            [alt-down option?]
                            [x (->long x)]
                            [y (->long y)]
                            [time-stamp (->long (* (tell #:type _double event timestamp) 1000.0))]
                            [caps-down (bit? modifiers NSAlphaShiftKeyMask)])])
                (unless (or wheel mod-change?)
                  (let ([alt-str (tell #:type _NSString event charactersIgnoringModifiers)])
                    (when (and (string? alt-str)
                               (= 1 (string-length alt-str)))
                      (let ([alt-code (string-ref alt-str 0)])
                        (unless (equal? alt-code (send k get-key-code))
                          (send k set-other-altgr-key-code alt-code)))))
                  (when (and (or (and option? 
                                      special-option-key?)
                                 (and control?
                                      (equal? (send k get-key-code) #\u00)))
                             (send k get-other-altgr-key-code))
                    ;; swap altenate with main
                    (let ([other (send k get-other-altgr-key-code)])
                      (send k set-other-altgr-key-code (send k get-key-code))
                      (send k set-key-code other))))
                (unless wheel
                  (unless (or down? (and mod-change?
                                         (case (send k get-key-code)
                                           [(shift rshift) (send k get-shift-down)]
                                           [(control rcontrol) (send k get-control-down)]
                                           [else #t])))
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
                                       #t))))
            result)))))))

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

(define-cocoa NSFilenamesPboardType _id)

(define _CGError _int32)
(define-appserv CGWarpMouseCursorPosition (_fun _NSPoint -> _CGError))
(define-appserv CGAssociateMouseAndMouseCursorPosition (_fun _BOOL -> _CGError))

(define window%
  (class object%
    (init-field parent 
                cocoa
                [no-show? #f])

    (define is-on? #f)
    (define accept-drag? #f)
    (define accept-parent-drag? #f)

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
    (define/public (fixup-locations-children)
      (void))
    (define/public (fix-dc) 
      (void))
    (define/public (paint-children)
      (void))

    (define/public (get-cocoa) cocoa)
    (define/public (get-cocoa-content) cocoa)
    (define/public (get-cocoa-focus) (get-cocoa-content))
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

    (define/public (set-parent p)
      (set! parent p))

    (define/public (get-eventspace) eventspace)

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

    (define/public (on-new-child child on?)
      (if on?
          (queue-window-event
           child
           (lambda ()
             (atomically
              (with-autorelease
               (send child child-accept-drag (or accept-drag? accept-parent-drag?))))))
          (send child child-accept-drag #f)))

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
      (atomically
       (set! enabled? on?)
       (when (is-parent-enabled-to-root?)
         (enable-window on?))))
    (define/public (enable-window on?)
      ;; in atomic mode
      (void))

    (define skip-enter-leave? #f)
    (define/public (skip-enter-leave-events skip?)
      (set! skip-enter-leave? skip?))

    (define block-all-mouse-events? #f)
    (define/public (block-mouse-events block?)
      (set! block-all-mouse-events? block?))

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

    (define event-position-wrt-wx #f)
    (define/public (set-event-positions-wrt wx)
      (set! event-position-wrt-wx wx))

    (define/public (is-view?) #t)
    (define/public (window-point-to-view pos)
      (let ([pos (if (is-view?)
                     (tell #:type _NSPoint (get-cocoa-content) 
                           convertPoint: #:type _NSPoint pos
                           fromView: #f)
                     pos)])
        (define x (NSPoint-x pos))
        (define y (flip-client (NSPoint-y pos)))
        (cond
         [event-position-wrt-wx
          (define xb (box (->long x)))
          (define yb (box (->long y)))
          (internal-client-to-screen xb yb)
          (send event-position-wrt-wx internal-screen-to-client xb yb)
          (values (unbox xb) (unbox yb))]
         [else (values x y)])))
                

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
      (let ([x (if (not x) (get-x) x)]
            [y (if (not y) (get-y) y)])
        ;; old location will need refresh:
        (tellv cocoa setNeedsDisplay: #:type _BOOL #t)
        (tellv cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint x (flip y h))
                                                           (make-NSSize w h)))
        ;; new location needs refresh:
        (tellv cocoa setNeedsDisplay: #:type _BOOL #t))
      (queue-on-size))

    (define/public (internal-move x y)
      (set-size x y (get-width) (get-height)))
    (define/public (move x y)
      (internal-move x y))

    (define/public (on-drop-file f) (void))
    (define/public (do-on-drop-file f)
      (if accept-drag?
          (on-drop-file (string->path f))
          (when parent
            (send parent do-on-drop-file f))))

    (define/public (drag-accept-files on?) 
      (unless (eq? (and on? #t) accept-drag?)
        (atomically
         (with-autorelease
          (set! accept-drag? (and on? #t))
          (accept-drags-everywhere (or accept-drag? accept-parent-drag?))))))

    (define/public (accept-drags-everywhere on?) 
      (if on?
          (tellv (get-cocoa-content) registerForDraggedTypes:
                 (let ([a (tell NSArray arrayWithObjects: #:type (_list i _id) (list NSFilenamesPboardType)
                                count: #:type _NSUInteger 1)])
                   a))
          (tellv (get-cocoa-content) unregisterDraggedTypes))
      (children-accept-drag on?))

    (define/public (children-accept-drag on?)
      (void))
    (define/public (child-accept-drag on?)
      (unless (eq? (and on? #t) accept-parent-drag?)
        (set! accept-parent-drag? (and on? #t))
        (accept-drags-everywhere (or accept-drag? accept-parent-drag?))))

    (define/public (set-focus)
      (when (and (gets-focus?)
                 (is-enabled-to-root?))
        (let ([w (tell cocoa window)])
          (when w
            (tellv w makeFirstResponder: (get-cocoa-focus))))))
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
      (flush))

    (define/public (flush)
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
       [(other-modal? this e) #t]
       [(call-pre-on-event this e) #t]
       [just-pre? block-all-mouse-events?]
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

    (define/public (post-mouse-down) (void))

    (define/public (on-char s) (void))
    (define/public (on-event m) (void))
    (define/public (queue-on-size) (void))

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

    (define/public (get-handle) (get-cocoa))
    (define/public (get-client-handle) (get-cocoa-content))

    (define/public (popup-menu m x y)
      (send m do-popup (get-cocoa-content) (get-cocoa-window) x (flip-client y)
            (lambda (thunk)
              (queue-window-event this thunk))))

    (define/public (center a b) (void))
    (define/public (refresh) (refresh-all-children))

    (define/public (refresh-all-children) (void))

    (define/public (screen-to-client xb yb)
      (internal-screen-to-client xb yb))
    (define/public (internal-screen-to-client xb yb)
      (let ([p (tell #:type _NSPoint (get-cocoa-content) 
                     convertPoint: #:type _NSPoint
                     (tell #:type _NSPoint (get-cocoa-window)
                           convertScreenToBase:
                           #:type _NSPoint (make-NSPoint (unbox xb) 
                                                         (send (get-wx-window) flip-screen (unbox yb))))
                     fromView: #f)])
        (set-box! xb (inexact->exact (floor (NSPoint-x p))))
        (set-box! yb (inexact->exact (floor (flip-client (NSPoint-y p)))))))

    (define/public (client-to-screen xb yb [flip-y? #t])
      (internal-client-to-screen xb yb flip-y?))
    (define/public (internal-client-to-screen xb yb [flip-y? #t])
      (let* ([p (tell #:type _NSPoint (get-cocoa-window)
                      convertBaseToScreen:
                      #:type _NSPoint
                      (tell #:type _NSPoint (get-cocoa-content) 
                            convertPoint: #:type _NSPoint
                            (make-NSPoint (unbox xb) (flip-client (unbox yb)))
                            toView: #f))])
        (let ([new-y (if flip-y?
                         (send (get-wx-window) flip-screen (NSPoint-y p))
                         (NSPoint-y p))])
          (set-box! xb (inexact->exact (floor (NSPoint-x p))))
          (set-box! yb (inexact->exact (floor new-y))))))
      
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
    (define/public (can-be-responder?) (is-enabled-to-root?))

    (define/public (on-color-change)
      (send parent on-color-change))

    ;; For multi-key character composition:
    (define saved-marked #f)
    (define saved-sel #f)
    (define/public (set-saved-marked v sel) 
      (set! saved-marked v) 
      (set! saved-sel sel)
      (if (and v 
               (not (string=? v ""))
               ;; Don't show the window for an empty string or certain
               ;; simple combinations (probably a better way than this);
               (not (member v '("¨" "ˆ" "´" "`" "˜"))))
          (create-compose-window)
          (when compose-cocoa
            (tellv compose-cocoa orderOut: #f))))
    (define/public (get-saved-marked) saved-marked)
    (define/public (get-saved-selected) saved-sel)

    (define/public (warp-pointer x y)
      (define xb (box x))
      (define yb (box y))
      (client-to-screen xb yb)
      (void (CGWarpMouseCursorPosition (make-NSPoint (unbox xb) (unbox yb))))
      (void (CGAssociateMouseAndMouseCursorPosition #t)))

    (define/private (create-compose-window)
      (unless compose-cocoa
        (set! compose-cocoa (tell (tell InputMethodPanel alloc)
                                  initWithContentRect: #:type _NSRect (make-NSRect
                                                                       (make-NSPoint 0 20)
                                                                       (make-NSSize 300 20))
                                  styleMask: #:type _int (bitwise-ior NSUtilityWindowMask
                                                                      NSResizableWindowMask
                                                                      NSClosableWindowMask)
                                  backing: #:type _int NSBackingStoreBuffered
                                  defer: #:type _BOOL NO))
        (set! compose-text (tell (tell NSTextView alloc)
                                 initWithFrame: #:type _NSRect (make-NSRect
                                                                (make-NSPoint 0 0)
                                                                (make-NSSize 10 10))))
        (tellv compose-cocoa setFloatingPanel: #:type _BOOL #t)
        (tellv (tell compose-cocoa contentView) addSubview: compose-text)
        (tellv compose-text sizeToFit)
        (tellv compose-cocoa setContentBorderThickness: #:type _CGFloat 5.0 forEdge: #:type _int 1)
        (let ([h (+ (NSSize-height
                     (NSRect-size
                      (tell #:type _NSRect
                            compose-cocoa frameRectForContentRect: 
                            #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                        (make-NSSize 0 0)))))
                    (NSSize-height (NSRect-size (tell #:type _NSRect compose-text frame))))])
          (tellv compose-cocoa setMinSize: #:type _NSSize (make-NSSize 1 h))
          (tellv compose-cocoa setMaxSize: #:type _NSSize (make-NSSize 32000 h))
          (tellv compose-cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 20)
                                                                     (make-NSSize 300 h))
                 display: #:type _BOOL #t))
        (reset-input-method-window-size)
        (tellv compose-cocoa setDelegate: compose-cocoa))
      (tellv compose-text 
             setMarkedText: #:type _NSString saved-marked 
             selectedRange: #:type _NSRange (make-NSRange (car saved-sel) (cdr saved-sel)))
      (tellv compose-cocoa orderFront: #f))))

(define (reset-input-method-window-size)
  (when compose-text
    (tell compose-text setFrame: #:type _NSRect
          (tell #:type _NSRect (tell compose-cocoa contentView) frame))))

(define compose-cocoa #f)
(define compose-text #f)

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
     (and (tell #:type _bool cocoa-win isVisible)
          (tellv cocoa-win disableFlushWindow)
          #t))
   (lambda (cocoa-win)
     (tellv cocoa-win enableFlushWindow))))

(define (cancel-flush-delay req)
  (do-cancel-flush-delay 
   req
   (lambda (cocoa-win)
     (tellv cocoa-win enableFlushWindow))))

(define (make-init-point x y)
  (make-NSPoint (if (not x)
                    0
                    x)
                (if (not y)
                    0
                    y)))

(define (flush-display)
  (try-to-sync-refresh)
  (for ([win (in-list (get-top-level-windows))])
    (send win flush)))
