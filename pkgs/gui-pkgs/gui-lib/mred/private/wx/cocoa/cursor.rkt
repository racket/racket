#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/draw
         "image.rkt"
         "types.rkt"
         "utils.rkt"
         "../common/cursor-draw.rkt"
         "../common/local.rkt")

(provide 
 (protect-out cursor-driver%
              arrow-cursor-handle
              get-wait-cursor-handle))

(import-class NSCursor)

(define wait #f)
(define bullseye #f)
(define blank #f)
(define size-ne/sw #f)
(define size-nw/se #f)

(define-syntax-rule (image-cursor id draw-proc)
  (or id
      (begin
        (set! id (make-image-cursor draw-proc))
        id)))

(define (make-image-cursor draw-proc)
  (let* ([bm (make-cursor-image draw-proc)])
    (let ([image (bitmap->image bm)])
      (tell (tell NSCursor alloc)
            initWithImage: image
            hotSpot: #:type _NSPoint (make-NSPoint 8 8)))))

(define arrow-cursor-handle (tell NSCursor arrowCursor))
(define (get-wait-cursor-handle)
  (image-cursor wait draw-watch))

(define cursor-driver% 
  (class object%
    (define handle #f)

    (define/public (set-standard sym)
      (case sym
        [(arrow)
         (set! handle arrow-cursor-handle)]
        [(cross)
         (set! handle (tell NSCursor crosshairCursor))]
        [(hand)
         (set! handle (tell NSCursor openHandCursor))]
        [(ibeam)
         (set! handle (tell NSCursor IBeamCursor))]
        [(size-n/s)
         (set! handle (tell NSCursor resizeUpDownCursor))]
        [(size-e/w)
         (set! handle (tell NSCursor resizeLeftRightCursor))]
        [(size-nw/se)
         (set! handle (image-cursor size-nw/se draw-nw/se))]
        [(size-ne/sw)
         (set! handle (image-cursor size-ne/sw draw-ne/sw))]
        [(watch)
         (set! handle (get-wait-cursor-handle))]
        [(bullseye)
         (set! handle (image-cursor bullseye draw-bullseye))]
        [(blank)
         (set! handle (image-cursor blank void))]))

    (define/public (set-image image mask hot-spot-x hot-spot-y)
      (let ([bm (make-object bitmap% 16 16 #f #t)])
        (let ([dc (make-object bitmap-dc% bm)])
          (send dc draw-bitmap image 0 0 'solid (send the-color-database find-color "black") mask)
          (send dc set-bitmap #f))
        (let ([image (bitmap->image bm)])
          (set! handle
                (as-objc-allocation
                 (tell (tell NSCursor alloc)
                       initWithImage: image
                       hotSpot: #:type _NSPoint (make-NSPoint hot-spot-x hot-spot-y)))))))
    
    (define/public (ok?) (and handle #t))

    (define/public (get-handle) handle)

    (super-new)))
