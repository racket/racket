#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
         racket/draw
         racket/draw/color
         "pool.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "dc.rkt"
         "queue.rkt"
         "../common/event.rkt"
         "../common/queue.rkt"
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/freeze.rkt")

(provide canvas%)

;; ----------------------------------------

(import-class NSView NSGraphicsContext NSScroller)

(define-objc-class MyView NSView 
  #:mixins (FocusResponder KeyMouseResponder) 
  [wx]
  (-a _void (drawRect: [_NSRect r])
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
            (tellv ctx restoreGraphicsState))))
      (send wx queue-paint)
      ;; ensure that `nextEventMatchingMask:' returns
      (post-dummy-event))
  (-a _void (viewWillMoveToWindow: [_id w])
      (when wx
        (queue-window-event wx (lambda () (send wx fix-dc)))))
  (-a _void (onHScroll: [_id scroller])
      (when wx (send wx do-scroll 'horizontal scroller)))
  (-a _void (onVScroll: [_id scroller])
      (when wx (send wx do-scroll 'vertical scroller))))

(define-struct scroller (cocoa [range #:mutable] [page #:mutable]))
(define scroll-width (tell #:type _CGFloat NSScroller scrollerWidth))

(define canvas%
  (class window%
    (init parent
          x y w h
          style
          [ignored-name #f]
          [gl-config #f])

    (inherit get-cocoa
             get-eventspace
             make-graphics-context
             get-client-size
             is-shown-to-root?
             move get-x get-y
             on-size
             register-as-child)

    (define vscroll-ok? (and (memq 'vscroll style) #t))
    (define vscroll? vscroll-ok?)
    (define hscroll-ok? (and (memq 'hscroll style) #t))
    (define hscroll? hscroll-ok?)

    (define canvas-style style)

    (define is-visible? #f)

    ;; Avoid multiple queued paints:
    (define paint-queued? #f)
    ;; To handle paint requests that happen while on-paint
    ;;  is being called already:
    (define now-drawing? #f)
    (define refresh-after-drawing? #f)

    (define/public (queue-paint)
      ;; can be called from any thread, including the event-pump thread
      (unless paint-queued?
        (set! paint-queued? #t)
        (queue-window-event this (lambda () 
                                   (set! paint-queued? #f)
                                   (when is-visible?
                                     (set! now-drawing? #t)
                                     (fix-dc)
                                     (on-paint)
                                     (set! now-drawing? #f)
                                     (when refresh-after-drawing?
                                       (set! refresh-after-drawing? #f)
                                       (refresh)))))))
    (define/override (refresh)
      (tellv content-cocoa setNeedsDisplay: #:type _BOOL #t))

    (define/override (get-cocoa-content) content-cocoa)

    (super-new 
     [parent parent]
     [cocoa
      (as-objc-allocation
       (tell (tell NSView alloc) 
             initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint x y)
                                                        (make-NSSize w h))))]
     [no-show? (memq 'deleted style)])

    (define cocoa (get-cocoa))

    (define content-cocoa 
      (as-objc-allocation
       (tell (tell MyView alloc) 
             initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                        (make-NSSize w h)))))
    (tell #:type _void cocoa addSubview: content-cocoa)
    (set-ivar! content-cocoa wx this)

    (define dc (make-object dc% (make-graphics-context) 0 0 10 10))

    (queue-paint)
    
    (define/public (get-dc) dc)

    (define/public (fix-dc)
      (let ([p (tell #:type _NSPoint content-cocoa 
                     convertPoint: #:type _NSPoint (make-NSPoint 0 0)
                     toView: #f)]
            [xb (box 0)]
            [yb (box 0)])
        (get-client-size xb yb)
        (send dc reset-bounds (NSPoint-x p) (NSPoint-y p) (unbox xb) (unbox yb))))

    (define/override (maybe-register-as-child parent on?)
      (register-as-child parent on?))

    (define/public (on-paint) (void))

    (define/override (set-size x y w h)
      (do-set-size x y w h))

    (define tr 0)

    (define/override (show on?)
      (set! is-visible? on?)
      ;; FIXME: what if we're in the middle of an on-paint?
      (super show on?))

    (define/private (do-set-size x y w h)
      (super set-size x y w h)
      (when tr
        (tellv content-cocoa removeTrackingRect: #:type _NSInteger tr)
        (set! tr #f))
      (let ([sz (make-NSSize (- w (if vscroll? scroll-width 0))
                             (- h (if hscroll? scroll-width 0)))]
            [pos (make-NSPoint 0 (if hscroll? scroll-width 0))])
        (tellv content-cocoa setFrame: #:type _NSRect (make-NSRect pos sz))
        (set! tr (tell #:type _NSInteger
                       content-cocoa
                       addTrackingRect: #:type _NSRect (make-NSRect (make-NSPoint 0 0) sz)
                       owner: content-cocoa
                       userData: #f
                       assumeInside: #:type _BOOL #f)))
      (when v-scroller
        (tellv (scroller-cocoa v-scroller) setFrame: #:type _NSRect
               (make-NSRect
                (make-NSPoint (- w scroll-width)
                              (if hscroll?
                                  scroll-width
                                  0))
                (make-NSSize scroll-width
                             (- h (if hscroll? scroll-width 0))))))
      (when h-scroller
        (tellv (scroller-cocoa h-scroller) setFrame: #:type _NSRect
               (make-NSRect
                (make-NSPoint 0 0)
                (make-NSSize (- w (if vscroll? scroll-width 0))
                             scroll-width))))
      (fix-dc)
      (on-size 0 0))
    (define/override (client-y-offset)
      (if hscroll?
          scroll-width
          0))

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
          (let ([r (tell #:type _NSRect cocoa frame)])
            (do-set-size (NSPoint-x (NSRect-origin r))
                         (NSPoint-y (NSRect-origin r))
                         (NSSize-width (NSRect-size r))
                         (NSSize-height (NSRect-size r)))))))

    (define/public (set-scrollbars h-step v-step
                                   h-len v-len
                                   h-page v-page
                                   h-pos v-pos
                                   auto?)
      (scroll-range h-scroller h-len)
      (scroll-page h-scroller h-page)
      (scroll-pos h-scroller h-pos)
      (when h-scroller
        (tell (scroller-cocoa h-scroller) setEnabled: #:type _BOOL (and h-step (positive? h-len))))
      (scroll-range v-scroller v-len)
      (scroll-page v-scroller v-page)
      (scroll-pos v-scroller v-pos)
      (when v-scroller
        (tell (scroller-cocoa v-scroller) setEnabled: #:type _BOOL (and v-step (positive? v-len)))))

    (define (update which scroll- v)
      (if (eq? which 'vertical)
          (scroll- v-scroller v)
          (scroll- h-scroller v)))

    (define/public (set-scroll-page which v)
      (update which scroll-page v))
    (define/public (set-scroll-range which v)
      (update which scroll-range v))
    (define/public (set-scroll-pos which v)
      (update which scroll-pos v))

    (define/public (get-scroll-page which) 
      (scroll-page (if (eq? which 'vertical) v-scroller h-scroller)))
    (define/public (get-scroll-range which)
      (scroll-range (if (eq? which 'vertical) v-scroller h-scroller)))
    (define/public (get-scroll-pos which)
      (scroll-pos (if (eq? which 'vertical) v-scroller h-scroller)))
    
    (define v-scroller
      (and vscroll-ok?
           (make-scroller
            (as-objc-allocation
             (tell (tell NSScroller alloc) initWithFrame: 
                   #:type _NSRect (make-NSRect
                                   (make-NSPoint (- w scroll-width)
                                                 (if hscroll?
                                                     scroll-width
                                                     0))
                                   (make-NSSize scroll-width
                                                (max (- h (if hscroll? scroll-width 0))
                                                     (+ scroll-width 10))))))
            1
            1)))
    (define h-scroller
      (and hscroll-ok?
           (make-scroller
            (as-objc-allocation
             (tell (tell NSScroller alloc) initWithFrame: 
                   #:type _NSRect (make-NSRect
                                   (make-NSPoint 0 0)
                                   (make-NSSize (max (- w (if vscroll? scroll-width 0))
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
            (->long (round (* (tell #:type _double (scroller-cocoa scroller) floatValue)
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
          (tellv (scroller-cocoa scroller) setKnobProportion:
                 #:type _CGFloat (max (min 1.0 (/ val 
                                                  (+ val (exact->inexact (scroller-range scroller)))))
                                      0.0)))]
       [(scroller)
        (if scroller
            (scroller-page scroller)
            1)]))

    (define/public (append-combo-item str) #f)
    (define/public (on-combo-select i) (void))

    (define bg-col (make-object color% "white"))
    (define/public (get-canvas-background) (if (memq 'transparent canvas-style)
                                               #f
                                               bg-col))
    (define/public (set-canvas-background col) (set! bg-col col))
    (define/public (get-canvas-background-for-clearing) 
      (if now-drawing?
          (begin
            (set! refresh-after-drawing? #t)
            #f)
          (and (not (memq 'transparent canvas-style)) 
               (not (memq 'no-autoclear canvas-style)) 
               bg-col)))

    (define/public (do-scroll direction scroller)
      ;; Called from the Cocoa handler thread
      (let ([part (tell #:type _int scroller hitPart)])
        (queue-window-event 
         this
         (lambda ()
           (let ([kind
                  (cond
                   [(= part NSScrollerDecrementPage)
                    (set-scroll-pos direction (- (get-scroll-pos direction)
                                                 (get-scroll-page direction)))
                    'page-up]
                   [(= part NSScrollerIncrementPage)
                    (set-scroll-pos direction (+ (get-scroll-pos direction)
                                                 (get-scroll-page direction)))
                    'page-down]
                   [(= part NSScrollerDecrementLine)
                    (set-scroll-pos direction (- (get-scroll-pos direction) 1))
                    'line-up]
                   [(= part NSScrollerIncrementLine)
                    (set-scroll-pos direction (+ (get-scroll-pos direction) 1))
                    'line-down]
                   [(= part NSScrollerKnob)
                    'thumb]
                   [else #f])])
             (when kind
               (on-scroll (new scroll-event%
                               [event-type kind]
                               [direction direction]
                               [position (get-scroll-pos direction)])))))))
      (constrained-reply (get-eventspace)
                         (lambda ()
                           (let loop () (pre-event-sync #t) (when (yield) (loop))))
                         (void)))
    (define/public (on-scroll e) (void))
    
    (define/override (wants-all-events?) 
      ;; Called in Cocoa event-handling mode
      #t)

    (def/public-unimplemented set-background-to-gray)
    (def/public-unimplemented scroll)
    (def/public-unimplemented warp-pointer)
    (def/public-unimplemented view-start)
    (define/public (set-resize-corner on?)
      (void))
    (def/public-unimplemented get-virtual-size)))
