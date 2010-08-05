#lang scheme/base
(require scheme/class
         scheme/foreign
         ffi/objc
          "../../syntax.rkt"
         "item.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "window.rkt"
         "../common/event.rkt"
         "../common/queue.rkt"
         "../common/freeze.rkt"
         "../../lock.rkt")
(unsafe!)
(objc-unsafe!)

(provide slider%)

;; ----------------------------------------

(import-class NSSlider)

(define-objc-class MySlider NSSlider
  #:mixins (FocusResponder KeyMouseResponder)
  [wx]
  (-a _void (changed: [_id sender])
      (queue-window-event wx (lambda () (send wx changed)))
      (constrained-reply
       (send wx get-eventspace)
       (lambda () (let loop () (pre-event-sync #t) (when (yield) (loop))))
       (void))))

(defclass slider% item%
  (init parent cb
        label
        val lo hi
        x y w
        style
        font)
  (inherit get-cocoa)

  (super-new [parent parent]
             [cocoa (let ([cocoa (tell (tell MySlider alloc) init)]
                          [vert? (memq 'vertical style)])
                      (tellv cocoa setMinValue: #:type _double* lo)
                      (tellv cocoa setMaxValue: #:type _double* hi)
                      (tellv cocoa setDoubleValue: #:type _double* val)
                      (tellv cocoa setNumberOfTickMarks: #:type _NSUInteger (add1 (- hi lo)))
                      (tellv cocoa setAllowsTickMarkValuesOnly: #:type _BOOL #t)
                      (tellv cocoa setFrame: #:type _NSRect (make-NSRect 
                                                             (make-NSPoint 0 0)
                                                             (make-NSSize (if vert? 24 32)
                                                                          (if vert? 32 24))))
                      (tellv cocoa setContinuous: #:type _BOOL #t)
                      ; (tellv cocoa sizeToFit)
                      cocoa)]
             [callback cb]
             [no-show? (memq 'deleted style)])

  (define cocoa (get-cocoa))

  (tellv cocoa setTarget: cocoa)
  (tellv cocoa setAction: #:type _SEL (selector changed:))

  (define callback cb)
  (define/public (changed)
    (callback this (new control-event%
                        [event-type 'slider]
                        [time-stamp (current-milliseconds)])))


  (define/public (set-value v)
    (tellv cocoa setDoubleValue: #:type _double* v))
  (define/public (get-value)
    (inexact->exact (floor (tell #:type _double cocoa doubleValue)))))

