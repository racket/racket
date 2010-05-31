#lang scheme/base
(require ffi/objc
         scheme/foreign
         scheme/class)
(unsafe!)
(objc-unsafe!)

(provide cursor-driver%)

(import-class NSCursor)

(define cursor-driver% 
  (class object%
    (define handle #f)

    (define/public (set-standard sym)
      (case sym
        [(arrow)
         (set! handle (tell NSCursor arrowCursor))]
        [(cross)
         (set! handle (tell NSCursor crosshairCursor))]
        [(hand)
         (set! handle (tell NSCursor openHandCursor))]
        [(ibeam)
         (set! handle (tell NSCursor IBeamCursor))]
        [(size-n/s)
         (set! handle (tell NSCursor resizeUpDownCursor))]
        [(size-e/w)
         (set! handle (tell NSCursor resizeLeftRightCursor))]))
    
    (define/public (ok?) (and handle #t))

    (super-new)))

