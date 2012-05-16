#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
          "../../syntax.rkt"
         "item.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "window.rkt"
         "queue.rkt"
         "../common/event.rkt"
         "../common/queue.rkt"
         "../common/freeze.rkt"
         "../../lock.rkt")

(provide 
 (protect-out slider%))

;; ----------------------------------------

(import-class NSSlider NSTextField NSView)

(define-objc-class RacketSlider NSSlider
  #:mixins (FocusResponder KeyMouseResponder CursorDisplayer)
  [wxb]
  (-a _void (changed: [_id sender])
      (let ([wx (->wx wxb)])
        (when wx
          (send wx update-message)
          (queue-window-event wx (lambda () (send wx changed)))
          (constrained-reply
           (send wx get-eventspace)
           (lambda () (let loop () (pre-event-sync #t) (when (yield/no-sync) (loop))))
           (void))))))

(defclass slider% item%
  (init parent cb
        label
        val lo hi
        x y w
        style
        font)
  (inherit get-cocoa register-as-child
           init-font)

  (define vert? (memq 'vertical style))

  (define slider-lo lo)
  (define slider-hi hi)

  (define slider-cocoa
    (let ([cocoa (as-objc-allocation
                  (tell (tell RacketSlider alloc) init))])
      (tellv cocoa setMinValue: #:type _double* lo)
      (tellv cocoa setMaxValue: #:type _double* hi)
      (tellv cocoa setDoubleValue: #:type _double* (flip val))
      ;; heuristic: show up to tick marks:
      (when ((- hi lo) . < . 64)
        (tellv cocoa setNumberOfTickMarks: #:type _NSUInteger (add1 (- hi lo)))
        (tellv cocoa setAllowsTickMarkValuesOnly: #:type _BOOL #t))
      (tellv cocoa setFrame: #:type _NSRect (make-NSRect 
                                             (make-NSPoint 0 0)
                                             (make-NSSize (if vert? 24 32)
                                                          (if vert? 64 24))))
      (tellv cocoa setContinuous: #:type _BOOL #t)
      ;; (tellv cocoa sizeToFit)
      cocoa))

  (define-values (message-cocoa message-w message-h)
    (if (memq 'plain style)
        (values #f #f #f)
        (let ([cocoa (as-objc-allocation
                      (tell (tell NSTextField alloc) init))])
          (init-font cocoa font)
          (tellv cocoa setSelectable: #:type _BOOL #f)
          (tellv cocoa setEditable: #:type _BOOL #f)
          (tellv cocoa setBordered: #:type _BOOL #f)
          (tellv cocoa setDrawsBackground: #:type _BOOL #f)
          (tellv cocoa setTitleWithMnemonic: #:type _NSString (format "~a" hi))
          (tellv cocoa sizeToFit)
          (let ([r1 (tell #:type _NSRect cocoa frame)])
            (tellv cocoa setTitleWithMnemonic: #:type _NSString (format "~a" lo))
            (tellv cocoa sizeToFit)
            (let ([r2 (tell #:type _NSRect cocoa frame)])
              (tellv cocoa setTitleWithMnemonic: #:type _NSString (format "~a" val))
              (values cocoa
                      (max (NSSize-width (NSRect-size r1))
                           (NSSize-width (NSRect-size r2)))
                      (max (NSSize-height (NSRect-size r1))
                           (NSSize-height (NSRect-size r2)))))))))

  (define cocoa
    (if message-cocoa
        (let* ([f (tell #:type _NSRect slider-cocoa frame)]
               [w (+ (if vert?
                         message-w
                         0)
                     (NSSize-width (NSRect-size f)))]
               [h (+ (if vert?
                         0
                         message-h)
                     (NSSize-height (NSRect-size f)))])
          (let ([cocoa (as-objc-allocation
                        (tell (tell NSView alloc)
                              initWithFrame: #:type _NSRect (make-NSRect 
                                                             (make-init-point x y)
                                                             (make-NSSize w h))))])
            (tellv cocoa addSubview: slider-cocoa)
            (tellv cocoa addSubview: message-cocoa)
            (arrange-parts w h)
            cocoa))
        slider-cocoa))

  (define/private (arrange-parts w h)
    (tellv slider-cocoa setFrame: #:type _NSRect (make-NSRect
                                                  (make-NSPoint 0
                                                                (if vert? 0 message-h))
                                                  (make-NSSize (- w (if vert? message-w 0))
                                                               (- h (if vert? 0 message-h)))))
    (tellv message-cocoa setFrame: #:type _NSRect (make-NSRect
                                                   (make-NSPoint (if vert? 
                                                                     (- w message-w)
                                                                     (/ (- w message-w) 2))
                                                                 (if vert?
                                                                     (/ (- h message-h) 2)
                                                                     0))
                                                   (make-NSSize message-w message-h))))

  (define/override (set-size x y w h)
    (super set-size x y w h)
    (when message-cocoa
      (arrange-parts w h)))
  
  (when message-cocoa
    (set-ivar! slider-cocoa wxb (->wxb this)))

  (super-new [parent parent]
             [cocoa cocoa]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (define/override (get-cocoa-control) slider-cocoa)

  (tellv slider-cocoa setTarget: slider-cocoa)
  (tellv slider-cocoa setAction: #:type _SEL (selector changed:))

  (define callback cb)
  (define/public (changed)
    (callback this (new control-event%
                        [event-type 'slider]
                        [time-stamp (current-milliseconds)])))

  (define/private (flip v)
    (if vert?
        (+ slider-lo (- slider-hi v))
        v))

  (define/public (set-value v)
    (atomically
     (tellv slider-cocoa setDoubleValue: #:type _double* (flip v))
     (update-message v)))
  (define/public (get-value)
    (flip (inexact->exact (floor (tell #:type _double slider-cocoa doubleValue)))))

  (define/public (update-message [val (get-value)])
    (tellv message-cocoa setTitleWithMnemonic: #:type _NSString (format "~a" val)))

  (inherit get-cocoa-window)
  (define/override (post-mouse-down)
    ;; For some reason, dragging a slider disabled mouse-moved
    ;;  events for the window, so turn them back on:
    (tellv (get-cocoa-window) setAcceptsMouseMovedEvents: #:type _BOOL #t))

  (define/override (maybe-register-as-child parent on?)
    (register-as-child parent on?)))

