#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/draw/private/color
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "queue.rkt")

(provide 
 (protect-out get-color-from-user))

(import-class NSColorPanel
              NSColor)

(define-cocoa NSDeviceRGBColorSpace _id)

(define (get-color-from-user mode)
  (promote-to-gui!)
  (cond
   [(eq? mode 'show)
    (tellv (tell NSColorPanel sharedColorPanel)
           orderFront: #f)]
   [(eq? mode 'get)
    (atomically
     (let ([c (tell (tell (tell NSColorPanel sharedColorPanel) color)
                    colorUsingColorSpaceName: NSDeviceRGBColorSpace)]
           [as-color (lambda (v)
                       (inexact->exact (floor (* 255.0 v))))])
       (make-object color%
                    (as-color
                     (tell #:type _CGFloat c redComponent))
                    (as-color
                     (tell #:type _CGFloat c greenComponent))
                    (as-color
                     (tell #:type _CGFloat c blueComponent)))))]
   [else
    (let ([p (tell NSColorPanel sharedColorPanel)]
          [color mode])
      (atomically
       (tellv p setColor: (tell NSColor
                                colorWithDeviceRed: #:type _CGFloat (/ (color-red color) 255.0)
                                green: #:type _CGFloat (/ (color-green color) 255.0)
                                blue: #:type _CGFloat (/ (color-blue color) 255.0)
                                alpha: #:type _CGFloat 1.0))))]))
