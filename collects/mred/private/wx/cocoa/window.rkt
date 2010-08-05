#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class
         "queue.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "keycode.rkt"
         "../common/event.rkt"
         "../common/queue.rkt"
         "../../syntax.rkt"
         "../common/freeze.rkt")
(unsafe!)
(objc-unsafe!)

(provide window%
         queue-window-event
         FocusResponder
         KeyMouseResponder)

(define-local-member-name flip-client)

;; ----------------------------------------

(define-objc-mixin (FocusResponder Superclass)
  [wx]
  [-a _BOOL (acceptsFirstResponder)
      #t]
  [-a _BOOL (becomeFirstResponder)
      (and (super-tell becomeFirstResponder)
           (begin
             (send wx focus-is-on #t)
             (queue-window-event wx (lambda ()
                                      (send wx on-set-focus)))
             #t))]
  [-a _BOOL (resignFirstResponder)
      (and (super-tell resignFirstResponder)
           (begin
             (send wx focus-is-on #f)
             (queue-window-event wx (lambda ()
                                      (send wx on-kill-focus)))
             #t))])

(define-objc-mixin (KeyMouseResponder Superclass)
  [wx]
  [-a _void (mouseDown: [_id event]) 
      (unless (do-mouse-event wx event 'left-down #t #f #f 'right-down)
        (super-tell #:type _void mouseDown: event))]
  [-a _void (mouseUp: [_id event]) 
      (unless (do-mouse-event wx event 'left-up #f #f #f 'right-up)
        (super-tell #:type _void mouseUp: event))]
  [-a _void (mouseDragged: [_id event]) 
      (unless (do-mouse-event wx event 'motion #t #f #f)
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
      (do-mouse-event wx event 'motion #f #f #f)]
  [-a _void (mouseEntered: [_id event]) 
      (unless (do-mouse-event wx event 'enter #f #f #f)
        (super-tell #:type _void mouseEntered: event))]
  [-a _void (mouseExited: [_id event]) 
      (unless (do-mouse-event wx event 'leave #f #f #f)
        (super-tell #:type _void mouseExited: event))]
  [-a _void (rightMouseDown: [_id event]) 
      (unless (do-mouse-event wx event 'right-down #f #f #t)
        (super-tell #:type _void rightMouseDown: event))]
  [-a _void (rightMouseUp: [_id event]) 
      (unless (do-mouse-event wx event 'right-up #f #f #f)
        (super-tell #:type _void rightMouseUp: event))]
  [-a _void (rightMouseDragged: [_id event]) 
      (unless (do-mouse-event wx event 'motion #f #f #t)
        (super-tell #:type _void rightMouseDragged: event))]
  [-a _void (otherMouseDown: [_id event]) 
      (unless (do-mouse-event wx event 'middle-down #f #t #f)
        (super-tell #:type _void otherMouseDown: event))]
  [-a _void (otherMouseUp: [_id event]) 
      (unless (do-mouse-event wx event 'middle-up #f #f #f)
        (super-tell #:type _void otherMouseUp: event))]
  [-a _void (otherMouseDragged: [_id event]) 
      (unless (do-mouse-event wx event 'motion #f #t #f)
        (super-tell #:type _void otherMouseDragged: event))]
  
  [-a _void (keyDown: [_id event])
      (unless (do-key-event wx event)
        (super-tell #:type _void keyDown: event))]
  [-a _void (insertText: [_NSString str])
      (queue-window-event wx (lambda ()
                               (send wx key-event-as-string str)))])

(define (do-key-event wx event)
  (let* ([modifiers (tell #:type _NSUInteger event modifierFlags)]
         [bit? (lambda (m b) (positive? (bitwise-and m b)))]
         [pos (tell #:type _NSPoint event locationInWindow)]
         [str (tell #:type _NSString event characters)])
    (let-values ([(x y) (send wx window-point-to-view pos)])
      (let ([k (new key-event%
                    [key-code (or
                               (map-key-code (tell #:type _ushort event keyCode))
                               (if (string=? "" str)
                                   #\nul
                                   (string-ref str 0)))]
                    [shift-down (bit? modifiers NSShiftKeyMask)]
                    [control-down (bit? modifiers NSControlKeyMask)]
                    [meta-down (bit? modifiers NSCommandKeyMask)]
                    [alt-down (bit? modifiers NSAlternateKeyMask)]
                    [x (->long x)]
                    [y (->long y)]
                    [time-stamp (->long (* (tell #:type _double event timestamp) 1000.0))]
                    [caps-down (bit? modifiers NSAlphaShiftKeyMask)])])
        (if (send wx definitely-wants-event? k)
            (begin
              (queue-window-event wx (lambda ()
                                       (send wx dispatch-on-char k #f)))
              #t)
            (constrained-reply (send wx get-eventspace)
                               (lambda () (send wx dispatch-on-char k #t))
                               #t))))))

(define (do-mouse-event wx event kind l? m? r? [ctl-kind kind])
  (let* ([modifiers (tell #:type _NSUInteger event modifierFlags)]
         [bit? (lambda (m b) (positive? (bitwise-and m b)))]
         [pos (tell #:type _NSPoint event locationInWindow)])
    (let-values ([(x y) (send wx window-point-to-view pos)]
                 [(control-down) (bit? modifiers NSControlKeyMask)])
      (let ([m (new mouse-event%
                    [event-type (if control-down ctl-kind kind)]
                    [left-down (and l? (not control-down))]
                    [middle-down m?]
                    [right-down (or r? (and l? control-down))]
                    [x (->long x)]
                    [y (->long y)]
                    [shift-down (bit? modifiers NSShiftKeyMask)]
                    [meta-down (bit? modifiers NSCommandKeyMask)]
                    [alt-down (bit? modifiers NSAlternateKeyMask)]
                    [time-stamp (->long (* (tell #:type _double event timestamp) 1000.0))]
                    [caps-down (bit? modifiers NSAlphaShiftKeyMask)])])
        (if (send wx definitely-wants-event? m)
            (begin
              (queue-window-event wx (lambda ()
                                       (send wx dispatch-on-event m #f)))
              #t)
            (constrained-reply (send wx get-eventspace)
                               (lambda () (send wx dispatch-on-event m #t))
                               #t))))))

(define window%
  (class object%
    (init-field parent 
                cocoa
                [no-show? #f])

    (super-new)

    (define eventspace (if parent
                           (send parent get-eventspace)
                           (current-eventspace)))

    (set-ivar! cocoa wx this)

    (unless no-show?
      (show #t)) 

    (define/public (focus-is-on on?) (void))

    (define/public (get-cocoa) cocoa)
    (define/public (get-cocoa-content) cocoa)
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

    (define/public (show on?)
      (if on?
          (tellv (send parent get-cocoa-content) addSubview: cocoa)
          (tellv cocoa removeFromSuperview))
      (maybe-register-as-child parent on?))
    (define/public (maybe-register-as-child parent on?)
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
      (tellv cocoa setFrame: #:type _NSRect (make-NSRect (make-NSPoint x (flip y h))
                                                         (make-NSSize w h))))
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

    (define/public (dispatch-on-char e just-pre?) 
      (cond
       [(other-modal? this) #t]
       [(call-pre-on-char this e) #t]
       [just-pre? #f]
       [else (when enabled? (on-char e)) #t]))
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

    (define/public (client-to-screen xb yb)
      (let* ([p (tell #:type _NSPoint (get-cocoa-window)
                      convertBaseToScreen:
                      #:type _NSPoint
                      (tell #:type _NSPoint (get-cocoa-content) 
                            convertPointToBase: #:type _NSPoint
                            (make-NSPoint (unbox xb) (flip-client (unbox yb)))))])
        (let ([new-y (send (get-wx-window) flip-screen (NSPoint-y p))])
          (set-box! xb (inexact->exact (floor (NSPoint-x p))))
          (set-box! yb (inexact->exact (floor new-y))))))
      
    (def/public-unimplemented fit)

    (define/public (set-cursor c) (void))

    (define/public (gets-focus?) #f)
    
    (def/public-unimplemented centre)))

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))
