#lang racket/base
(require racket/class
         ffi/unsafe
         racket/math
         ffi/unsafe/objc
          "../../syntax.rkt"
         "item.rkt"
         "types.rkt"
         "const.rkt"
         "utils.rkt"
         "window.rkt")

(provide 
 (protect-out gauge%))

;; ----------------------------------------

(import-class NSProgressIndicator)

(define-objc-class RacketProgressIndicator NSProgressIndicator
  #:mixins (KeyMouseResponder CursorDisplayer)
  [wxb])

(defclass gauge% item%
  (init parent
        label
        rng
        x y w h
        style
        font)
  (inherit get-cocoa)

  (super-new [parent parent]
             [cocoa (let ([cocoa (as-objc-allocation
                                  ;; Beware that a gauge may be finally deallocated in 
                                  ;; a separate OS-level thread
                                  (tell (tell RacketProgressIndicator alloc) init))])
                      (tellv cocoa setIndeterminate: #:type _BOOL #f)
                      (tellv cocoa setMaxValue: #:type _double* rng)
                      (tellv cocoa setDoubleValue: #:type _double* 0.0)
                      (tellv cocoa sizeToFit)
                      (when (memq 'vertical style)
                        (let ([r (tell #:type _NSRect cocoa frame)])
                          (printf "height ~s\n" (NSSize-height (NSRect-size r)))
                          (tellv cocoa setFrame: 
                                 #:type _NSRect (make-NSRect
                                                 (NSRect-origin r)
                                                 (make-NSSize
                                                  (NSSize-height (NSRect-size r))
                                                  (NSSize-width (NSRect-size r)))))
                          (tellv cocoa rotateByAngle: #:type _CGFloat -90)))
                      cocoa)]
             [callback void]
             [no-show? (memq 'deleted style)])
  
  (define cocoa (get-cocoa))

  (define/override (enable on?) (void))
  (define/override (is-window-enabled?) #t)

  (define/public (get-range)
    (inexact->exact (floor (tell #:type _double cocoa maxValue))))
  (define/public (set-range rng)
    (tellv cocoa setMaxValue: #:type _double* rng)
    (tellv cocoa setDoubleValue: #:type _double* (min rng (tell #:type _double cocoa doubleValue))))

  (define/public (set-value v)
    (tellv cocoa setDoubleValue: #:type _double* v))
  (define/public (get-value)
    (min (inexact->exact (floor (tell #:type _double cocoa doubleValue)))
         (get-range))))
