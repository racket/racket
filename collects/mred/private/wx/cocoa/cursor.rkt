#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/draw
         "image.rkt"
         "types.rkt"
         "../common/local.rkt")

(provide cursor-driver%
         arrow-cursor-handle
         get-wait-cursor-handle)

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
  (let* ([bm (make-object bitmap% 16 16 #f #t)]
         [dc (make-object bitmap-dc% bm)])
    (send dc set-smoothing 'aligned)
    (draw-proc dc 16 16)
    (send dc set-bitmap #f)
    (let ([image (bitmap->image bm)])
      (tell (tell NSCursor alloc)
            initWithImage: image
            hotSpot: #:type _NSPoint (make-NSPoint 8 8)))))

(define arrow-cursor-handle (tell NSCursor arrowCursor))
(define (get-wait-cursor-handle)
  (image-cursor wait
                (lambda (dc w h)
                  (send dc set-brush "black" 'solid)
                  (send dc draw-rectangle 5 0 6 4)
                  (send dc draw-rectangle 5 12 6 4)
                  (send dc set-brush "white" 'solid)
                  (send dc draw-ellipse 3 3 10 10)
                  (send dc draw-line 7 5 7 8)
                  (send dc draw-line 7 8 9 8))))

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
         (set! handle
               (image-cursor size-nw/se (lambda (dc w h)
                                          (send dc draw-line 0 16 16 0)
                                          (send dc draw-line 0 0 16 16)
                                          (send dc draw-line 0 3 0 0)
                                          (send dc draw-line 0 0 3 0)
                                          (send dc draw-line 12 15 15 15)
                                          (send dc draw-line 15 15 15 12))))]
        [(size-ne/sw)
         (set! handle
               (image-cursor size-ne/sw (lambda (dc w h)
                                          (send dc draw-line 0 16 16 0)
                                          (send dc draw-line 0 0 16 16)
                                          (send dc draw-line 12 0 15 0)
                                          (send dc draw-line 15 0 15 3)
                                          (send dc draw-line 0 12 0 15)
                                          (send dc draw-line 0 15 3 15))))]
        [(watch)
         (set! handle (get-wait-cursor-handle))]
        [(bullseye)
         (set! handle
               (image-cursor bullseye (lambda (dc w h)
                                        (send dc draw-ellipse 1 1 (- w 2) (- h 2))
                                        (send dc draw-ellipse 4 4 (- w 8) (- h 8))
                                        (send dc draw-ellipse 7 7 2 2))))]
        [(blank)
         (set! handle (image-cursor blank void))]))
    
    (define/public (ok?) (and handle #t))

    (define/public (get-handle) handle)

    (super-new)))
