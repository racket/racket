#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/alloc
         racket/draw
         racket/draw/private/local
         racket/draw/unsafe/cairo
         "../../lock.rkt"
         racket/draw/unsafe/bstr
         "utils.rkt"
         "types.rkt"
         (only-in '#%foreign ffi-callback))

(provide 
 (protect-out bitmap->pixbuf
              pixbuf->bitmap
              
              _GdkPixbuf
              gtk_image_new_from_pixbuf
              release-pixbuf))

(define _GdkPixbuf (_cpointer/null 'GdkPixbuf))

(define release-pixbuf ((deallocator) g_object_unref))

(define-gtk gtk_image_new_from_pixbuf (_fun _GdkPixbuf -> _GtkWidget))
(define-gdk_pixbuf gdk_pixbuf_new_from_data (_fun _pointer ; data
						  _int ; 0  =RGB
						  _gboolean ; has_alpha?
						  _int ; bits_per_sample
						  _int ; width
						  _int ; height
						  _int ; rowstride
						  _fpointer ; destroy
						  _pointer  ; destroy data
						  -> _GdkPixbuf)
  #:wrap (allocator release-pixbuf))

(define-gdk gdk_cairo_set_source_pixbuf (_fun _cairo_t _GdkPixbuf _double* _double* -> _void))
(define-gdk_pixbuf gdk_pixbuf_get_width (_fun _GdkPixbuf -> _int))
(define-gdk_pixbuf gdk_pixbuf_get_height (_fun _GdkPixbuf -> _int))

(define free-it (ffi-callback free
                              (list _pointer)
                              _void
                              #f
                              #t))

(define (bitmap->pixbuf bm)
  (let* ([w (send bm get-width)]
         [h (send bm get-height)]
         [str (make-bytes (* w h 4) 255)])
    (send bm get-argb-pixels 0 0 w h str #f)
    (let ([mask (send bm get-loaded-mask)])
      (when mask
        (send mask get-argb-pixels 0 0 w h str #t)))
    (atomically
     (let ([rgba (scheme_make_sized_byte_string (malloc (* w h 4) 'raw) (* w h 4) 0)])
       (memcpy rgba (ptr-add str 1) (sub1 (* w h 4)))
       (for ([i (in-range 0 (* w h 4) 4)])
         (bytes-set! rgba (+ i 3) (bytes-ref str i)))
       (gdk_pixbuf_new_from_data rgba
                                 0
                                 #t
                                 8
                                 w
                                 h
                                 (* w 4)
                                 free-it
                                 #f)))))

(define (pixbuf->bitmap pixbuf)
  (let* ([w (gdk_pixbuf_get_width pixbuf)]
         [h (gdk_pixbuf_get_height pixbuf)]
         [bm (make-object bitmap% w h #f #t)]
         [s (send bm get-cairo-surface)]
         [cr (cairo_create s)])
    (gdk_cairo_set_source_pixbuf cr pixbuf 0 0)
    (cairo_rectangle cr 0 0 w h)
    (cairo_fill cr)
    (cairo_destroy cr)
    bm))
