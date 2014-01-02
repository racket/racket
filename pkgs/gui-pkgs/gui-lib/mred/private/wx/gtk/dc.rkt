#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
         "window.rkt"
         "x11.rkt"
         "win32.rkt"
         "gl-context.rkt"
	 "../../lock.rkt"
         "../common/backing-dc.rkt"
         racket/draw/unsafe/cairo
         racket/draw/private/dc
         racket/draw/private/bitmap
         racket/draw/private/local
         ffi/unsafe/alloc)

(provide 
 (protect-out dc%
              do-backing-flush
              x11-bitmap%

              gdk_gc_new
              gdk_gc_unref
              gdk_gc_set_rgb_fg_color
              gdk_draw_rectangle))

(define-gdk gdk_cairo_create (_fun _pointer -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define-gdk gdk_gc_unref (_fun _pointer -> _void)
  #:wrap (deallocator))
(define-gdk gdk_gc_new (_fun _GdkWindow -> _pointer)
  #:wrap (allocator gdk_gc_unref))
(define-gdk gdk_gc_set_rgb_fg_color (_fun _pointer _GdkColor-pointer -> _void))
(define-gdk gdk_draw_rectangle (_fun _GdkWindow _pointer _gboolean _int _int _int _int -> _void))

(define-cstruct _GdkVisual-rec ([type-instance _pointer]
				[ref_count _uint]
				[qdata _pointer]
				[type _int]
				[depth _int]))
(define-gdk gdk_visual_get_system (_fun -> _GdkVisual-rec-pointer))

(define x11-bitmap%
  (class bitmap%
    (init w h gdk-win)
    (super-make-object (make-alternate-bitmap-kind w h 1.0))

    (define pixmap (gdk_pixmap_new gdk-win 
				   (min (max 1 w) 32000)
				   (min (max 1 h) 32000)
				   (if gdk-win 
				       -1
				       (GdkVisual-rec-depth
					(gdk_visual_get_system)))))
    (define s
      (cairo_xlib_surface_create (gdk_x11_display_get_xdisplay
                                  (gdk_drawable_get_display pixmap))
                                 (gdk_x11_drawable_get_xid pixmap)
				 (gdk_x11_visual_get_xvisual
				  (gdk_drawable_get_visual pixmap))
                                 w
                                 h))

    ;; initialize bitmap to white:
    (let ([cr (cairo_create s)])
      (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
      (cairo_paint cr)
      (cairo_destroy cr))

    ;; `get-gdk-pixmap' and `install-gl-context' are
    ;; localized in "gl-context.rkt"
    (define/public (get-gdk-pixmap) pixmap)
    (define/public (install-gl-context new-gl) (set! gl new-gl))

    (define gl #f)
    (define/override (get-bitmap-gl-context) gl)

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
      (if (not gdk-win)
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
    (init [(cnvs canvas)]
          transparent?)
    (inherit end-delay)
    (define canvas cnvs)

    (super-new [transparent? transparent?])

    (define gl #f)
    (define/override (get-gl-context)
      (or gl
          (let ([v (create-widget-gl-context (send canvas get-client-gtk))])
	    (when v (set! gl v))
	    v)))

    (define/override (make-backing-bitmap w h)
      (cond
       [(and (eq? 'unix (system-type))
             (send canvas get-canvas-background))
	(make-object x11-bitmap% w h (widget-window (send canvas get-client-gtk)))]
       [(and (eq? 'windows (system-type))
             (send canvas get-canvas-background))
	(make-object win32-bitmap% w h (widget-window (send canvas get-client-gtk)))]
       [else
	(super make-backing-bitmap (max 1 w) (max 1 h))]))

    (define/override (get-backing-size xb yb)
      (send canvas get-client-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      ;; Re-enable expose events so that the queued 
      ;; backing flush will be handled:
      (end-delay)
      (send canvas queue-backing-flush))

    (define/override (flush)
      (send canvas flush))

    (define/override (request-delay)
      (request-flush-delay (send canvas get-flush-window)))
    (define/override (cancel-delay req)
      (cancel-flush-delay req))))

(define (do-backing-flush canvas dc win)
  (send dc on-backing-flush
        (lambda (bm)
          (let ([w (box 0)]
                [h (box 0)])
            (send canvas get-client-size w h)
            (let ([cr (gdk_cairo_create win)])
              (backing-draw-bm bm cr (unbox w) (unbox h))
              (cairo_destroy cr))))))
