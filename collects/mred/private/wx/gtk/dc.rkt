#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "x11.rkt"
         "win32.rkt"
	 "../../lock.rkt"
         "../common/backing-dc.rkt"
         racket/draw/cairo
         racket/draw/dc
         racket/draw/bitmap
         racket/draw/local
         ffi/unsafe/alloc)

(provide dc%
         do-backing-flush
         x11-bitmap%)

(define-gdk gdk_cairo_create (_fun _pointer -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define x11-bitmap%
  (class bitmap%
    (init w h gdk-win)
    (super-make-object (make-alternate-bitmap-kind w h))

    (define pixmap (gdk_pixmap_new gdk-win w h (if gdk-win -1 24)))
    (define s
      (cairo_xlib_surface_create (gdk_x11_display_get_xdisplay
                                  (gdk_drawable_get_display pixmap))
                                 (gdk_x11_drawable_get_xid pixmap)
                                 (gdk_x11_visual_get_xvisual
                                  (gdk_drawable_get_visual pixmap))
                                 w
                                 h))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)
    
    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (gobject-unref pixmap)
       (set! s #f)))))

(define win32-bitmap%
  (class bitmap%
    (init w h gdk-win)
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s
      (if gdk-win
	  (cairo_win32_surface_create_with_dib CAIRO_FORMAT_RGB24 w h)
	  (atomically
	   (let ([hdc (GetDC (gdk_win32_drawable_get_handle gdk-win))])
	     (begin0
	      (cairo_win32_surface_create_with_ddb hdc
						   CAIRO_FORMAT_RGB24 w h)
	      (ReleaseDC hdc))))))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)

    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (set! s #f)))))

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)])
    (define canvas cnvs)

    (super-new)

    (define/override (make-backing-bitmap w h)
      (cond
       [(and (eq? 'unix (system-type))
             (send canvas get-canvas-background))
	(make-object x11-bitmap% w h (widget-window (send canvas get-client-gtk)))]
       [(and (eq? 'windows (system-type))
             (send canvas get-canvas-background))
	(make-object win32-bitmap% w h (widget-window (send canvas get-client-gtk)))]
       [else
	(super make-backing-bitmap w h)]))

    (define/override (get-backing-size xb yb)
      (send canvas get-client-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      ;; called atomically (not expecting exceptions)
      (send canvas queue-backing-flush))

    (define/override (request-delay)
      (request-flush-delay (send canvas get-flush-window)))
    (define/override (cancel-delay req)
      (cancel-flush-delay req))))

(define (do-backing-flush canvas dc win)
  (begin0
   (send dc on-backing-flush
         (lambda (bm)
           (let ([w (box 0)]
                 [h (box 0)])
             (send canvas get-client-size w h)
             (let ([cr (gdk_cairo_create win)])
               (let ([s (cairo_get_source cr)])
                 (cairo_pattern_reference s)
                 (cairo_set_source_surface cr (send bm get-cairo-surface) 0 0)
                 (cairo_new_path cr)
                 (cairo_rectangle cr 0 0 (unbox w) (unbox h))
                 (cairo_fill cr)
                 (cairo_set_source cr s)
                 (cairo_pattern_destroy s))
               (cairo_destroy cr)))))
   (send dc end-delay)))
