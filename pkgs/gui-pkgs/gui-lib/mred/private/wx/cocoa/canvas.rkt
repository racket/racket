#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
         racket/draw
         racket/draw/private/gl-context
         (except-in racket/draw/private/color
                    color% make-color)
         (only-in racket/draw/private/bitmap quartz-bitmap%)
         "pool.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "dc.rkt"
         "cg.rkt"
         "queue.rkt"
         "item.rkt"
         "gc.rkt"
         "image.rkt"
         "panel.rkt"
         "../common/backing-dc.rkt"
         "../common/canvas-mixin.rkt"
         "../common/event.rkt"
         "../common/queue.rkt"
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/freeze.rkt")

(provide 
 (protect-out canvas%
              canvas-panel%))

;; ----------------------------------------

(import-class NSView NSGraphicsContext NSScroller NSComboBox NSWindow 
              NSImageView NSTextFieldCell 
              NSOpenGLView NSOpenGLContext NSOpenGLPixelFormat)

(import-protocol NSComboBoxDelegate)

(define NSWindowAbove 1)

(define o (current-error-port))

;; Called when a canvas has no backing store ready
(define (clear-background wxb)
  (let ([wx (->wx wxb)])
    (when wx
      (let ([bg (send wx get-canvas-background-for-clearing)])
        (when bg
          (let ([ctx (tell NSGraphicsContext currentContext)])
            (tellv ctx saveGraphicsState)
            (let ([cg (tell #:type _CGContextRef ctx graphicsPort)]
                  [adj (lambda (v) (/ v 255.0))])
              (CGContextSetRGBFillColor cg
                                        (adj (color-red bg))
                                        (adj (color-blue bg))
                                        (adj (color-green bg))
                                        1.0)
              (CGContextFillRect cg (make-NSRect (make-NSPoint 0 0)
                                                 (make-NSSize 32000 32000))))
            (tellv ctx restoreGraphicsState)))))))

(define-objc-mixin (RacketViewMixin Superclass)
  #:mixins (KeyMouseTextResponder CursorDisplayer FocusResponder) 
  [wxb]
  (-a #:async-apply (box (void))
      _void (drawRect: [_NSRect r])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (send wx drawing-requested)
            (unless (send wx paint-or-queue-paint)
              (clear-background wxb)
              ;; ensure that `nextEventMatchingMask:' returns
              (post-dummy-event))))))
  (-a _void (viewWillMoveToWindow: [_id w])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (queue-window-event wx (lambda () (send wx fix-dc)))))))
  (-a _void (onHScroll: [_id scroller])
      (when wxb 
        (let ([wx (->wx wxb)])
          (when wx (send wx do-scroll 'horizontal scroller)))))
  (-a _void (onVScroll: [_id scroller])
      (when wxb 
        (let ([wx (->wx wxb)])
          (when wx (send wx do-scroll 'vertical scroller))))))

(define-objc-class RacketView NSView 
  #:mixins (RacketViewMixin)
  [wxb])

(define-objc-class RacketGLView NSOpenGLView
  #:mixins (RacketViewMixin)
  [wxb])

(define-objc-class CornerlessFrameView NSView 
  []
  (-a #:async-apply (box (void))
      _void (drawRect: [_NSRect r])
      (let ([ctx (tell NSGraphicsContext currentContext)])
        (tellv ctx saveGraphicsState)
        (let ([cg (tell #:type _CGContextRef ctx graphicsPort)]
              [r (tell #:type _NSRect self bounds)])
          (CGContextSetRGBFillColor cg 0 0 0 1.0)
          (let* ([l (NSPoint-x (NSRect-origin r))]
                 [t (NSPoint-y (NSRect-origin r))]
                 [b (+ t (NSSize-height (NSRect-size r)))]
                 [r (+ l (NSSize-width (NSRect-size r)))])
            (CGContextAddLines cg
                               (vector
                                (make-NSPoint r (+ t scroll-width))
                                (make-NSPoint r b)
                                (make-NSPoint l b)
                                (make-NSPoint l t)
                                (make-NSPoint (- r scroll-width) t))))
          (CGContextStrokePath cg))
        (tellv ctx restoreGraphicsState))))

(define-cocoa NSSetFocusRingStyle (_fun _int -> _void))
(define-cocoa NSRectFill (_fun _NSRect -> _void))

(define bezel-cell
  (tell (tell NSTextFieldCell alloc) initTextCell: #:type _NSString ""))
(tellv bezel-cell setBezeled: #:type _BOOL #t)

(define-objc-class FocusView NSView 
  [on?]
  (-a _void (setFocusState: [_BOOL is-on?])
      (set! on? is-on?))
  (-a #:async-apply (box (void))
      _void (drawRect: [_NSRect r])
      (let ([f (tell #:type _NSRect self frame)])
        (tellv bezel-cell 
               drawWithFrame: #:type _NSRect (make-NSRect (make-NSPoint 2 2)
                                                          (let ([s (NSRect-size r)])
                                                            (make-NSSize (- (NSSize-width s) 4)
                                                                         (- (NSSize-height s) 4))))
               inView: self))
      (when on?
        (let ([ctx (tell NSGraphicsContext currentContext)])
          (tellv ctx saveGraphicsState)
          (NSSetFocusRingStyle 0)
          (let ([r (tell #:type _NSRect self bounds)])
            (NSRectFill (make-NSRect (make-NSPoint
                                      (+ (NSPoint-x (NSRect-origin r)) 2)
                                      (+ (NSPoint-y (NSRect-origin r)) 2))
                                     (make-NSSize
                                      (- (NSSize-width (NSRect-size r)) 4)
                                      (- (NSSize-height (NSRect-size r)) 4)))))
          (tellv ctx restoreGraphicsState)))))

(define-objc-class RacketComboBox NSComboBox
  #:mixins (FocusResponder KeyMouseTextResponder CursorDisplayer) 
  #:protocols (NSComboBoxDelegate)
  [wxb]
  (-a #:async-apply (box (void))
      _void (drawRect: [_NSRect r])
      (super-tell #:type _void drawRect: #:type _NSRect r)
      (let ([wx (->wx wxb)])
        (when wx
          (unless (send wx paint-or-queue-paint)
            (unless (send wx during-menu-click?)
              (clear-background wxb)
              ;; ensure that `nextEventMatchingMask:' returns
              (post-dummy-event))))))
  (-a _void (comboBoxWillPopUp: [_id notification])
      (let ([wx (->wx wxb)])
        (when wx
          (send wx starting-combo))))
  (-a _void (comboBoxWillDismiss: [_id notification])
      (let ([wx (->wx wxb)])
        (when wx
          (send wx ending-combo))))
  (-a _void (viewWillMoveToWindow: [_id w])
      (when wxb
        (let ([wx (->wx wxb)])
          (when wx
            (queue-window-event wx (lambda () (send wx fix-dc))))))))

(define NSOpenGLPFADoubleBuffer 5)
(define NSOpenGLPFAStereo 6)
(define NSOpenGLPFAColorSize 8)
(define NSOpenGLPFAAlphaSize 11)
(define NSOpenGLPFADepthSize 12)
(define NSOpenGLPFAStencilSize 13)
(define NSOpenGLPFAAccumSize 14)
(define NSOpenGLPFAOffScreen 53)
(define NSOpenGLPFASampleBuffers 55)
(define NSOpenGLPFASamples 56)
(define NSOpenGLPFAMultisample 59)

(define (gl-config->pixel-format conf)
  (let ([conf (or conf (new gl-config%))])
    (tell (tell NSOpenGLPixelFormat alloc)
          initWithAttributes: #:type (_list i _int)
          (append
           (if (send conf get-double-buffered) (list NSOpenGLPFADoubleBuffer) null)
           (if (send conf get-stereo) (list NSOpenGLPFAStereo) null)
           (list
            NSOpenGLPFADepthSize (send conf get-depth-size)
            NSOpenGLPFAStencilSize (send conf get-stencil-size)
            NSOpenGLPFAAccumSize (send conf get-accum-size))
           (let ([ms (send conf get-multisample-size)])
             (if (zero? ms)
                 null
                 (list NSOpenGLPFAMultisample
                       NSOpenGLPFASampleBuffers 1
                       NSOpenGLPFASamples ms)))
           (list 0)))))
        
  
(define-struct scroller (cocoa [range #:mutable] [page #:mutable]))
(define scroll-width (tell #:type _CGFloat NSScroller scrollerWidth))

;; Customizing by version is a terrible idea, but I can't figure
;; out the right way to get the content area of an NSComboBox
(define combo-dx (if (version-10.7-or-later?)
                     2
                     2))
(define combo-dy (if (version-10.7-or-later?)
                     4
                     2))
(define combo-dw (if (version-10.7-or-later?)
                     24
                     22))
(define combo-dh (if (version-10.7-or-later?)
                     6
                     5))
;; extra height shaved off drawing; we can't just increase
;; combo-dh, because that just creates a request for an even taller
;; combo box whose primitive drawing overlaps the focus ring
(define combo-backing-dh 1)

(define canvas%
  (canvas-mixin
   (class (canvas-autoscroll-mixin window%)
     (init parent
           x y w h
           style
           [ignored-name #f]
           [gl-config #f])

     (inherit get-cocoa get-cocoa-window
              get-eventspace
              make-graphics-context
              is-shown-to-root?
              is-shown-to-before-root?
              is-enabled-to-root?
              is-window-enabled?
              block-mouse-events
              move get-x get-y
              register-as-child
              get-size get-position
              set-focus
              client-to-screen
              is-auto-scroll? is-disabled-scroll? 
              get-virtual-width get-virtual-height
              reset-auto-scroll
              refresh-for-autoscroll
              refresh-all-children
              flush)

     (define vscroll-ok? (and (or (memq 'vscroll style) 
                                  (memq 'auto-vscroll style)) ; 'auto variant falls through from panel
                              #t))
     (define vscroll? vscroll-ok?)
     (define hscroll-ok? (and (or (memq 'hscroll style) 
                                  (memq 'auto-hscroll style))
                              #t))
     (define hscroll? hscroll-ok?)

     (define wants-focus? (and (not (memq 'no-focus style))
                               (not (is-panel?))))
     (define is-combo? (memq 'combo style))
     (define has-control-border? (and (not is-combo?)
                                      (memq 'control-border style)))

     (define-values (x-margin y-margin x-sb-margin y-sb-margin)
       (cond
        [has-control-border? (values 3 3 3 3)]
        [(memq 'border style) (values 1 1 0 0)]
        [else (values 0 0 0 0)]))

     (define canvas-style style)

     (define/override (focus-is-on on?)
       (when has-control-border?
         (tellv cocoa setFocusState: #:type _BOOL on?)
         (tellv cocoa setNeedsDisplay: #:type _BOOL #t))
       (super focus-is-on on?))

     ;; The `queue-paint' and `paint-children' methods
     ;; are defined by `canvas-mixin' from ../common/canvas-mixin
     (define/public (queue-paint) (void))
     (define/public (request-canvas-flush-delay)
       (unless is-gl?
         (request-flush-delay (get-cocoa-window))))
     (define/public (cancel-canvas-flush-delay req)
       (unless is-gl?
         (cancel-flush-delay req)))
     (define/public (queue-canvas-refresh-event thunk)
       (queue-window-refresh-event this  thunk))
     (define/public (skip-pre-paint?) 
       (cond
        [is-gl?
         ;; We can't use GL on the window until it is ready,
         ;; as indicated by a request to draw.
         (unless drawing-requested?
           (sync/timeout 0.1 drawing-requested-sema))
         (not drawing-requested?)]
        [else #f]))

     (define drawing-requested? #f)
     (define drawing-requested-sema (make-semaphore))
     (define/public (drawing-requested) 
       (unless drawing-requested?
         (set! drawing-requested? #t)
         (semaphore-post drawing-requested-sema)))

     (define/public (paint-or-queue-paint)
       (cond
        [is-gl? (do-canvas-backing-flush #f)
                (queue-paint)
                #t]
        [(do-canvas-backing-flush #f) #t]
        [else (queue-paint)
              #f]))

     (define/public (do-canvas-backing-flush ctx)
       (do-backing-flush this dc (tell NSGraphicsContext currentContext)
                         (if is-combo? combo-dx 0) (if is-combo? combo-dy 0)))

     ;; not used, because Cocoa canvas refreshes do not go through
     ;;  the eventspace queue:
     (define/public (schedule-periodic-backing-flush)
       (void))

     (define/public (begin-refresh-sequence)
       (send dc suspend-flush))
     (define/public (end-refresh-sequence)
       (send dc resume-flush))

     (define/public (get-flush-window)
       (get-cocoa-window))

     (define/private (refresh-one)
       (when is-gl?
         (tellv content-cocoa setNeedsDisplay: #:type _BOOL #t))
       (queue-paint))
     (define/override (refresh)
       ;; can be called from any thread, including the event-pump thread
       (refresh-one)
       (refresh-all-children))

     (define/public (queue-backing-flush)
       (unless is-gl?
         ;; called atomically (not expecting exceptions)
         (tellv content-cocoa setNeedsDisplay: #:type _BOOL #t)))

     (define/override (get-cocoa-content) content-cocoa)

     (define is-gl? (and (not is-combo?) (memq 'gl style)))
     (define/public (can-gl?) is-gl?)

     (define dc #f)
     (define blits null)
     (define reg-blits null)

     (super-new 
      [parent parent]
      [cocoa
       (as-objc-allocation
        (tell (tell (cond
                     [is-combo? NSView]
                     [has-control-border? FocusView]
                     [(memq 'border style) (if (memq 'vscroll style)
                                               CornerlessFrameView
                                               FrameView)]
                     [else NSView])
                    alloc) 
              initWithFrame: #:type _NSRect (make-NSRect (make-init-point x y)
                                                         (make-NSSize (max w (* 2 x-margin))
                                                                      (max h (* 2 y-margin))))))]
      [no-show? (memq 'deleted style)])

     (define cocoa (get-cocoa))

     (define content-cocoa
       (let ([r (make-NSRect (make-NSPoint 0 0)
                             (make-NSSize (max 0 (- w (if vscroll? scroll-width 0) (* 2 x-margin)))
                                          (max 0 (- h (if hscroll? scroll-width 0) (* 2 y-margin)))))])
         (as-objc-allocation
          (if (or is-combo? (not (memq 'gl style)))
              (tell (tell (if is-combo? RacketComboBox RacketView)
                          alloc)
                    initWithFrame: #:type _NSRect r)
              (let* ([share-context (and gl-config (send gl-config get-share-context))]
                     [context-handle (and share-context (send share-context get-handle))]
                     [pf (gl-config->pixel-format gl-config)]
                     [new-context (and
                                   context-handle
                                   (tell (tell NSOpenGLContext alloc)
                                         initWithFormat: pf
                                         shareContext: context-handle))]
                     [gl-view (tell (tell RacketGLView alloc)
                                    initWithFrame: #:type _NSRect r
                                    pixelFormat: pf)])
                (when new-context
                  (tellv gl-view setOpenGLContext: new-context)
                  ;; We're supposed to sync via `setView:' but it fails,
                  ;; perhaps because the view isn't yet visible:
                  ;;   (tellv new-context setView: gl-view)
                  (tellv new-context release))
                (tellv pf release)
                gl-view)))))
     (tell #:type _void cocoa addSubview: content-cocoa)
     (set-ivar! content-cocoa wxb (->wxb this))

     (when is-combo?
       (tellv content-cocoa setEditable: #:type _BOOL #f)
       (tellv content-cocoa setDelegate: content-cocoa)
       (install-control-font content-cocoa #f))

     (set! dc (make-object dc% this (memq 'transparent canvas-style)))

     (send dc start-backing-retained)

     (queue-paint)

     (define/public (is-panel?) #f)
     
     (define/public (get-dc) dc)

     (define/public (make-compatible-bitmap w h)
       (make-window-bitmap w h (get-cocoa-window)))

     (define/override (fix-dc [refresh? #t])
       (when (dc . is-a? . dc%)
         (send dc reset-backing-retained)
         (send dc set-auto-scroll
               (if (is-auto-scroll?) (scroll-pos h-scroller) 0)
               (if (is-auto-scroll?) (scroll-pos v-scroller) 0)))
       (when refresh? (refresh-one)))

     (define/override (get-client-size xb yb)
       (super get-client-size xb yb)
       (when is-combo?
         (set-box! xb (max 0 (- (unbox xb) combo-dw)))
         (set-box! yb (max 0 (- (unbox yb) combo-dh)))))

     (define/override (maybe-register-as-child parent on?)
       (register-as-child parent on?))

     (define/public (on-paint) (void))

     (define/override (set-size x y w h)
       (do-set-size x y w h))

     (define tr 0)

     (define/override (show on?)
       ;; FIXME: what if we're in the middle of an on-paint?
       (super show on?))

     (define/override (hide-children)
       (super hide-children)
       (fix-dc #f)
       (suspend-all-reg-blits))

     (define/override (show-children)
       (super show-children)
       ;; (fix-dc) ; inteferes with `paint-children''
       (resume-all-reg-blits))

     (define/override (fixup-locations-children)
       ;; in atomic mode
       (suspend-all-reg-blits)
       (resume-all-reg-blits))

     (define/private (do-set-size x y w h)
       (when (pair? blits)
         (atomically (suspend-all-reg-blits)))
       (super set-size x y w h)
       (when tr
         (tellv content-cocoa removeTrackingRect: #:type _NSInteger tr)
         (set! tr #f))
       (let ([sz (make-NSSize (- w (if vscroll? scroll-width 0) x-margin x-margin)
                              (- h (if hscroll? scroll-width 0) y-margin y-margin))]
             [pos (make-NSPoint x-margin (+ (if hscroll? scroll-width 0) y-margin))])
         (tellv content-cocoa setFrame: #:type _NSRect (make-NSRect pos sz))
         (set! tr (tell #:type _NSInteger
                        content-cocoa
                        addTrackingRect: #:type _NSRect (make-NSRect (make-NSPoint x-margin y-margin) sz)
                        owner: content-cocoa
                        userData: #f
                        assumeInside: #:type _BOOL #f)))
       (when v-scroller
         (tellv (scroller-cocoa v-scroller) setFrame: #:type _NSRect
                (make-NSRect
                 (make-NSPoint (- w scroll-width x-sb-margin)
                               (+ (if hscroll?
                                      scroll-width
                                      0)
                                  y-sb-margin))
                 (make-NSSize scroll-width
                              (max 0 (- h (if hscroll? scroll-width 0)
                                        x-sb-margin x-sb-margin))))))
       (when h-scroller
         (tellv (scroller-cocoa h-scroller) setFrame: #:type _NSRect
                (make-NSRect
                 (make-NSPoint x-sb-margin y-sb-margin)
                 (make-NSSize (max 0 (- w (if vscroll? scroll-width 0)
                                        x-sb-margin x-sb-margin))
                              scroll-width))))
       (when (and (pair? blits)
                  (is-shown-to-root?))
         (atomically (resume-all-reg-blits)))
       (fix-dc)
       (when (and (is-auto-scroll?)
                  (not (is-panel?)))
         (reset-auto-scroll 0 0))
       (on-size))

     ;; this `on-size' method is for `editor-canvas%', only:
     (define/public (on-size) (void))

     (define/public (show-scrollbars h? v?)
       (let ([h? (and h? hscroll-ok?)]
             [v? (and v? vscroll-ok?)])
         (unless (and (eq? h? hscroll?)
                      (eq? v? vscroll?))
           (cond
            [(and h? (not hscroll?))
             (tell #:type _void cocoa addSubview: (scroller-cocoa h-scroller))]
            [(and hscroll? (not h?))
             (tell #:type _void (scroller-cocoa h-scroller) removeFromSuperview)])
           (set! hscroll? h?)
           (cond
            [(and v? (not vscroll?))
             (tell #:type _void cocoa addSubview: (scroller-cocoa v-scroller))]
            [(and vscroll? (not v?))
             (tell #:type _void (scroller-cocoa v-scroller) removeFromSuperview)])
           (set! vscroll? v?)
           (let ([x (box 0)] [y (box 0)] [w (box 0)] [h (box 0)])
             (get-position x y)
             (get-size w h)
             (do-set-size (unbox x) (unbox y) (unbox w) (unbox h))))))

     (define/override (do-set-scrollbars h-step v-step
                                         h-len v-len
                                         h-page v-page
                                         h-pos v-pos)
       (scroll-range h-scroller h-len)
       (scroll-page h-scroller h-page)
       (unless (= h-pos -1)
         (scroll-pos h-scroller h-pos))
       (when h-scroller
         (tellv (scroller-cocoa h-scroller) setEnabled: #:type _BOOL (and h-step (positive? h-len))))
       (scroll-range v-scroller v-len)
       (scroll-page v-scroller v-page)
       (unless (= v-pos -1)
         (scroll-pos v-scroller v-pos))
       (when v-scroller
         (tellv (scroller-cocoa v-scroller) setEnabled: #:type _BOOL (and v-step (positive? v-len)))))

     (define/override (reset-dc-for-autoscroll)
       (fix-dc))

     (define/private (update which scroll- v)
       (if (eq? which 'vertical)
           (scroll- v-scroller v)
           (scroll- h-scroller v)))

     (define/public (set-scroll-page which v)
       (update which scroll-page v))
     (define/public (set-scroll-range which v)
       (update which scroll-range v))
     (define/public (set-scroll-pos which v)
       (update which scroll-pos v))

     (define/private (guard-scroll skip-guard? which get-v)
       (if skip-guard?
           (get-v)
           (if (or (if (eq? which 'vertical)
                       (not vscroll-ok?)
                       (not hscroll-ok?))
                   (is-disabled-scroll?)
                   (is-auto-scroll?))
               0
               (get-v))))

     (define/public (get-scroll-page which [skip-guard? #f]) 
       (guard-scroll skip-guard?
                     which
                     (lambda ()
                       (scroll-page (if (eq? which 'vertical) v-scroller h-scroller)))))
     (define/public (get-scroll-range which [skip-guard? #f])
       (guard-scroll skip-guard?
                     which
                     (lambda ()
                       (scroll-range (if (eq? which 'vertical) v-scroller h-scroller)))))
     (define/public (get-scroll-pos which [skip-guard? #f])
       (guard-scroll skip-guard?
                     which
                     (lambda ()
                       (scroll-pos (if (eq? which 'vertical) v-scroller h-scroller)))))
     
     (define v-scroller
       (and vscroll-ok?
            (make-scroller
             (as-objc-allocation
              (tell (tell NSScroller alloc) initWithFrame: 
                    #:type _NSRect (make-NSRect
                                    (make-NSPoint (- w scroll-width x-sb-margin)
                                                  (+ (if hscroll?
                                                         scroll-width
                                                         0)
                                                     y-sb-margin))
                                    (make-NSSize scroll-width
                                                 (max (- h (if hscroll? scroll-width 0)
                                                         y-sb-margin y-sb-margin)
                                                      (+ scroll-width 10))))))
             1
             1)))
     (define h-scroller
       (and hscroll-ok?
            (make-scroller
             (as-objc-allocation
              (tell (tell NSScroller alloc) initWithFrame: 
                    #:type _NSRect (make-NSRect
                                    (make-NSPoint x-sb-margin y-sb-margin)
                                    (make-NSSize (max (- w (if vscroll? scroll-width 0)
                                                         x-sb-margin x-sb-margin)
                                                      (+ scroll-width 10))
                                                 scroll-width))))
             1
             1)))

     (when v-scroller
       (tell #:type _void cocoa addSubview: (scroller-cocoa v-scroller))
       (tellv (scroller-cocoa v-scroller) setTarget: content-cocoa)
       (tellv (scroller-cocoa v-scroller) setAction: #:type _SEL (selector onVScroll:)))
     (when h-scroller
       (tell #:type _void cocoa addSubview: (scroller-cocoa h-scroller))
       (tellv (scroller-cocoa h-scroller) setTarget: content-cocoa)
       (tellv (scroller-cocoa h-scroller) setAction: #:type _SEL (selector onHScroll:)))
     
     (define scroll-pos
       (case-lambda
        [(scroller val)
         (when scroller
           (tellv (scroller-cocoa scroller) setFloatValue:
                  #:type _float (max (min 1.0 (/ val (exact->inexact (scroller-range scroller))))
                                     0.0)))]
        [(scroller)
         (if scroller
             (->long (round (* (tell #:type _float (scroller-cocoa scroller) floatValue)
                               (scroller-range scroller))))
             0)]))

     (define scroll-range
       (case-lambda
        [(scroller val)
         (when scroller
           (let ([pos (scroll-pos scroller)]
                 [page (scroll-page scroller)])
             (set-scroller-range! scroller val)
             (tell (scroller-cocoa scroller) setEnabled: #:type _BOOL (positive? val))
             (scroll-pos scroller pos)
             (scroll-page scroller page)))]
        [(scroller) 
         (if scroller
             (scroller-range scroller)
             1)]))
     
     (define scroll-page
       (case-lambda
        [(scroller val)
         (when scroller
           (set-scroller-page! scroller val)
           (let ([proportion
                  (max (min 1.0 (/ val 
                                   (+ val (exact->inexact (scroller-range scroller)))))
                       0.0)])
             (if old-cocoa?
                 (tellv (scroller-cocoa scroller) 
                        setFloatValue: #:type _float (tell #:type _float (scroller-cocoa scroller)  
                                                           floatValue)
                        knobProportion: #:type _CGFloat proportion)
                 (tellv (scroller-cocoa scroller) setKnobProportion:
                        #:type _CGFloat proportion))))]
        [(scroller)
         (if scroller
             (scroller-page scroller)
             1)]))

     (define/override (enable-window on?)
       ;; in atomic mode
       (let ([on? (and on? (is-window-enabled?))])
         (let ([w (tell content-cocoa window)])
           (when (ptr-equal? content-cocoa (tell w firstResponder))
             (tellv w makeFirstResponder: #f)))
         (block-mouse-events (not on?))
         (when is-combo?
           (tellv content-cocoa setEnabled: #:type _BOOL on?))))

     (define/public (clear-combo-items)
       (tellv content-cocoa removeAllItems))
     (define/public (append-combo-item str)
       (tellv content-cocoa addItemWithObjectValue: #:type _NSString str)
       #t)
     (define/public (on-combo-select i) (void))
     (define/public (popup-combo)
       ;; Pending refresh events interfere with combo popups
       ;; for some reason, so flush them:
       (yield-refresh)
       (flush)
       ;; Beware that the `popUp:' method is undocumented:
       (atomically
        (tellv (tell content-cocoa cell) popUp: #f)))

     (define clear-bg? (and (not (memq 'transparent canvas-style)) 
                            (not (memq 'no-autoclear canvas-style))))
     (define bg-col (make-object color% "white"))
     (define/public (get-canvas-background) (if (memq 'transparent canvas-style)
                                                #f
                                                bg-col))
     (define/public (set-canvas-background col) (set! bg-col col))
     (define/public (get-canvas-background-for-backing) (and clear-bg? bg-col))
     (define/public (get-canvas-background-for-clearing) 
       (and clear-bg?
            bg-col))

     (define/public (reject-partial-update r)
       ;; Called in the event-pump thread.
       ;; A transparent canvas cannot handle a partial update.
       (and (or 
             ;; Multiple clipping rects?
             (let ([i (malloc _NSInteger)]
                   [r (malloc 'atomic _pointer)])
               (tellv content-cocoa getRectsBeingDrawn: #:type _pointer r 
                      count: #:type _pointer i)
               ((ptr-ref i _NSInteger) . > . 1))
             ;; Single clipping not whole area?
             (let ([s1 (NSRect-size (tell #:type _NSRect content-cocoa frame))]
                   [s2 (NSRect-size r)])
               (or ((NSSize-width s2) . < . (NSSize-width s1))
                   ((NSSize-height s2) . < . (NSSize-height s1)))))
            (begin
              (queue-window-event this (lambda () (refresh-one)))
              #t)))

     (define/public (do-scroll direction scroller)
       ;; Called from the Cocoa handler thread
       (let ([part (tell #:type _int scroller hitPart)])
         (queue-window-event 
          this
          (lambda ()
            (let ([kind
                   (cond
                    [(= part NSScrollerDecrementPage)
                     (set-scroll-pos direction (- (get-scroll-pos direction #t)
                                                  (get-scroll-page direction #t)))
                     'page-up]
                    [(= part NSScrollerIncrementPage)
                     (set-scroll-pos direction (+ (get-scroll-pos direction #t)
                                                  (get-scroll-page direction #t)))
                     'page-down]
                    [(= part NSScrollerDecrementLine)
                     (set-scroll-pos direction (- (get-scroll-pos direction #t) 1))
                     'line-up]
                    [(= part NSScrollerIncrementLine)
                     (set-scroll-pos direction (+ (get-scroll-pos direction #t) 1))
                     'line-down]
                    [(= part NSScrollerKnob)
                     'thumb]
                    [else #f])])
              (when kind
                (if (is-auto-scroll?)
                    (refresh-for-autoscroll)
                    (on-scroll (new scroll-event%
                                    [event-type kind]
                                    [direction direction]
                                    [position (get-scroll-pos direction)]))))))))
       (constrained-reply (get-eventspace)
                          (lambda ()
                            (let loop () (pre-event-sync #t) (when (yield/no-sync) (loop))))
                          (void)))
     (define/public (on-scroll e) (void))
     
     (define/override (definitely-wants-event? e) 
       ;; Called in Cocoa event-handling mode
       (when (and wants-focus?
                  (e . is-a? . mouse-event%)
                  (send e button-down? 'left))
         (set-focus))
       (and (not (is-panel?))
            (or (not is-combo?)
                (e . is-a? . key-event%)
                (not (send e button-down? 'left))
                (not (on-menu-click? e)))))

     (define/override (gets-focus?)
       wants-focus?)
     (define/override (can-be-responder?)
       (and wants-focus? (is-enabled-to-root?)))

     (define/private (on-menu-click? e)
       ;; Called in Cocoa event-handling mode
       (let ([xb (box 0)]
             [yb (box 0)])
         (get-client-size xb yb)
         ((send e get-x) . > . (unbox xb))))

     (define/public (on-popup) (void))

     (define/public (starting-combo)
       (set! in-menu-click? #t)
       (tellv content-cocoa setStringValue: #:type _NSString current-text)
       (constrained-reply (get-eventspace)
                          (lambda () (on-popup))
                          (void)))
       
     (define/public (ending-combo)
       (set! in-menu-click? #f)
       (let ([pos (tell #:type _NSInteger content-cocoa indexOfSelectedItem)])
         (when (pos . > . -1)
           (queue-window-event this (lambda () (on-combo-select pos)))))
       (refresh-one))

     (define current-text "")
     (define/public (set-combo-text t)
       (set! current-text t))

     (define in-menu-click? #f)

     (define/public (during-menu-click?)
       ;; Called in Cocoa event-handling mode
       in-menu-click?)

     (define/public (scroll x y)
       (when (is-auto-scroll?) 
         (when (x . >= . 0) (scroll-pos h-scroller (floor (* x (scroll-range h-scroller)))))
         (when (y . >= . 0) (scroll-pos v-scroller (floor (* y (scroll-range v-scroller)))))
         (refresh-for-autoscroll)))

     (define/override (get-virtual-h-pos)
       (scroll-pos h-scroller))

     (define/override (get-virtual-v-pos)
       (scroll-pos v-scroller))

     (define/public (set-resize-corner on?)
       (void))

     (define/public (get-backing-size xb yb)
       (get-client-size xb yb)
       (when is-combo?
         (set-box! yb (max 0 (- (unbox yb) combo-backing-dh)))))

     (define/override (get-cursor-width-delta)
       0)

     (define/public (is-flipped?)
       (tell #:type _BOOL (get-cocoa-content) isFlipped))

     (define/private (suspend-all-reg-blits)
       (let ([cocoa-win (get-cocoa-window)])
         (for ([r (in-list reg-blits)])
           (tellv cocoa-win removeChildWindow: (car r))
           (release (car r))
           (scheme_remove_gc_callback (cdr r))))
       (set! reg-blits null))

     (define/public (resume-all-reg-blits)
       (unless (pair? reg-blits)
         (when (pair? blits)
           (set! reg-blits
                 (for/list ([b (in-list blits)])
                   (let-values ([(x y w h img) (apply values b)])
                     (register-one-blit x y w h img)))))))

     (define/private (register-one-blit x y w h img)
       (let ([xb (box x)]
             [yb (box y)])
         (client-to-screen xb yb #f)
         (let* ([cocoa-win (get-cocoa-window)])
           (atomically
            (let ([win (as-objc-allocation
                        (tell (tell NSWindow alloc)
                              initWithContentRect: #:type _NSRect (make-NSRect (make-NSPoint (unbox xb)
                                                                                             (- (unbox yb)
                                                                                                h))
                                                                               (make-NSSize w h))
                              styleMask: #:type _int NSBorderlessWindowMask
                              backing: #:type _int NSBackingStoreBuffered
                              defer: #:type _BOOL NO))]
                  [iv (tell (tell NSImageView alloc) init)])
              (tellv iv setImage: img)
              (tellv iv setFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                              (make-NSSize w h)))
              (tellv (tell win contentView) addSubview: iv)
              (tellv win setAlphaValue: #:type _CGFloat 0.0)
              (tellv cocoa-win addChildWindow: win ordered: #:type _int NSWindowAbove)
              (tellv iv release)
              (let ([r (scheme_add_gc_callback
                        (make-gc-action-desc win (selector setAlphaValue:) 1.0)
                        (make-gc-action-desc win (selector setAlphaValue:) 0.0))])
                (cons win r)))))))
     
     (define/public (register-collecting-blit x y w h on off on-x on-y off-x off-y)
       (let ([on (fix-bitmap-size on w h on-x on-y)])
         (let ([img (bitmap->image on)])
           (atomically
            (set! blits (cons (list x y w h img) blits))
            (when (is-shown-to-root?)
              (set! reg-blits (cons (register-one-blit x y w h img) reg-blits)))))))

     (define/public (unregister-collecting-blits)
       (atomically
        (suspend-all-reg-blits)
        (set! blits null))))))

(define canvas-panel%
  (class (panel-mixin canvas%)
    (inherit get-virtual-h-pos
             get-virtual-v-pos
             get-cocoa-content)

    (define/override (is-panel?) #t)

    (define/override (reset-dc-for-autoscroll)
      (let* ([content-cocoa (get-cocoa-content)])
        (tellv content-cocoa setBoundsOrigin: #:type _NSPoint
               (make-NSPoint (get-virtual-h-pos)
                             (- (get-virtual-v-pos)))))
      (super reset-dc-for-autoscroll))

    (super-new)))
