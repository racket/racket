#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw
         "utils.rkt"
         "types.rkt"
         "pixbuf.rkt"
         "../common/cursor-draw.rkt"
          "../../syntax.rkt")

(provide 
 (protect-out cursor-driver%
              get-arrow-cursor-handle
              get-watch-cursor-handle))

(define GDK_ARROW 2) ; ugly!
(define GDK_CROSSHAIR 34)
(define GDK_HAND2 60)
(define GDK_SB_H_DOUBLE_ARROW 108)
(define GDK_SB_V_DOUBLE_ARROW 116)
(define GDK_XTERM 152)
(define GDK_TARGET 128)
(define GDK_WATCH 150)

(define gdk-cursors
  (make-hasheq (list
                (cons 'arrow GDK_ARROW)
                (cons 'cross GDK_CROSSHAIR)
                (cons 'ibeam GDK_XTERM)
                (cons 'bullseye GDK_TARGET)
                (cons 'watch 150)
                (cons 'size-e/w GDK_SB_H_DOUBLE_ARROW)
                (cons 'size-n/s GDK_SB_V_DOUBLE_ARROW)
                (cons 'size-ne/sw draw-ne/sw)
                (cons 'size-nw/se draw-nw/se)
                (cons 'blank void)
                (cons 'hand GDK_HAND2))))

(define _GdkCursor (_cpointer 'GdkCursor))
(define-gdk gdk_cursor_new  (_fun _int -> _GdkCursor))
(define-gdk gdk_display_get_default (_fun -> _GdkDisplay))
(define-gdk gdk_cursor_new_from_pixbuf (_fun _GdkDisplay _GdkPixbuf _int _int -> _GdkCursor))

(define (get-arrow-cursor-handle)
  (hash-ref gdk-cursors 'arrow #f))

(define (get-watch-cursor-handle)
  (let ([v (hash-ref gdk-cursors 'watch #f)])
    (if (number? v)
        (begin
          (send (new cursor-driver%) set-standard 'watch)
          (get-watch-cursor-handle))
        v)))

(defclass cursor-driver% object%

  (define handle #f)

  (define/public (ok?) (and handle #t))

  (define/public (set-standard sym)
    (let ([v (hash-ref gdk-cursors sym #f)])
      (cond
       [(not v) (void)]
       [(number? v)
        (let ([c (gdk_cursor_new v)])
          (hash-set! gdk-cursors sym c)
          (set! handle c))]
       [(procedure? v)
        (let ([bm (make-cursor-image v)])
          (let ([c (gdk_cursor_new_from_pixbuf
                    (gdk_display_get_default)
                    (bitmap->pixbuf bm)
                    8
                    8)])
            (hash-set! gdk-cursors sym c)
            (set! handle c)))]
       [else (set! handle v)])))

  (define/public (set-image image mask hot-spot-x hot-spot-y)
    (let ([bm (make-object bitmap% 16 16 #f #t)])
      (let ([dc (make-object bitmap-dc% bm)])
        (send dc draw-bitmap image 0 0 'solid (send the-color-database find-color "black") mask)
        (send dc set-bitmap #f))
      (let ([pixbuf (bitmap->pixbuf bm)])
        (set! handle
              (gdk_cursor_new_from_pixbuf
               (gdk_display_get_default)
               pixbuf
               hot-spot-x
               hot-spot-y)))))

  (define/public (get-handle) handle)
    
  (super-new))
