#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt"
	 "icons.rkt"
         "../common/cursor-draw.rkt"
          "../../syntax.rkt")

(provide 
 (protect-out cursor-driver%
              get-arrow-cursor
              get-wait-cursor))

(define-user32 CreateCursor (_wfun _HINSTANCE
                                   _int ; x
                                   _int ; y
                                   _int ; width
                                   _int ; height
                                   _pointer ; AND
                                   _pointer ; XOR
                                   -> _HCURSOR))

(define handles (make-hasheq))
(define (load-cursor num)
  (or (hash-ref handles num #f)
      (let ([h (LoadCursorW #f num)])
        (hash-set! handles num h)
        h)))

(define (get-arrow-cursor)
  (load-cursor IDC_ARROW))
(define (get-wait-cursor)
  (load-cursor IDC_APPSTARTING))

(defclass cursor-driver% object%
  (define handle #f)

  (define/public (set-standard sym)
    (case sym
      [(arrow)
       (set! handle (load-cursor IDC_ARROW))]
      [(cross)
       (set! handle (load-cursor IDC_CROSS))]
      [(hand)
       (set! handle (load-cursor IDC_HAND))]
      [(ibeam)
       (set! handle (load-cursor IDC_IBEAM))]
      [(size-n/s)
       (set! handle (load-cursor IDC_SIZENS))]
      [(size-e/w)
       (set! handle (load-cursor IDC_SIZEWE))]
      [(size-nw/se)
       (set! handle (load-cursor IDC_SIZENWSE))]
      [(size-ne/sw)
       (set! handle (load-cursor IDC_SIZENESW))]
      [(watch)
       (set! handle (load-cursor IDC_APPSTARTING))]
      [(bullseye)
       (set-image (make-cursor-image draw-bullseye 'unsmoothed) #f 8 8)]
      [(blank)
       (set-image #f #f 0 0)]))

  (define/public (set-image image mask hot-spot-x hot-spot-y
                            [ai (make-bytes (/ (* 16 16) 8) 255)]
                            [xi (make-bytes (/ (* 16 16) 8) 0)])
    (let ([s (make-bytes (* 16 16 4) 0)])
      (when image
        (send image get-argb-pixels 0 0 16 16 s)
        (if mask
            (send mask get-argb-pixels 0 0 16 16 s #t)
            (send image get-argb-pixels 0 0 16 16 s #t)))
      (for* ([i (in-range 16)]
             [j (in-range 16)])
        (let ([pos (* 4 (+ (* j 16) i))])
          (when (positive? (bytes-ref s pos))
            ;; black bit in mask
            (let ([bpos (+ (* j (/ 16 8)) (quotient i 8))]
                  [bit (arithmetic-shift 1 (- 7 (modulo i 8)))])
              (bytes-set! ai bpos (- (bytes-ref ai bpos) bit))
              (unless (and (zero? (bytes-ref s (+ 1 pos)))
                           (zero? (bytes-ref s (+ 2 pos)))
                           (zero? (bytes-ref s (+ 3 pos))))
                ;; white cursor pixel
                (bytes-set! xi bpos (+ (bytes-ref xi bpos) bit)))))))
      (set! handle
            (CreateCursor hInstance hot-spot-x hot-spot-y
                          16 16
                          ai xi))))

  (define/public (ok?) (and handle #t))
  (define/public (get-handle) handle)

  (super-new))
