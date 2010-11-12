#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "utils.rkt")

(provide 
 (protect-out gdk_pixmap_new
              gdk_drawable_get_display
              gdk_drawable_get_visual
              gdk_x11_drawable_get_xid
              gdk_x11_display_get_xdisplay
              gdk_x11_visual_get_xvisual))

(define _GdkDrawable _pointer)
(define _GdkDisplay (_cpointer 'GdkDisplay))
(define _GdkVisual (_cpointer 'GdkVisual))
(define _GdkPixmap (_cpointer 'GdkPixmap))
(define _Visual (_cpointer 'Visual))
(define _Display (_cpointer 'Display))
(define _Drawable _ulong)

(define-gdk gdk_pixmap_new (_fun _GdkDrawable _int _int _int -> _GdkPixmap)
  #:wrap (allocator gobject-unref))

(define-gdk gdk_drawable_get_display (_fun _GdkDrawable -> _GdkDisplay))
(define-gdk gdk_drawable_get_visual (_fun _GdkDrawable -> _GdkVisual))

(define-gdk gdk_x11_drawable_get_xid (_fun _GdkDrawable -> _Drawable)
  #:make-fail make-not-available)

(define-gdk gdk_x11_display_get_xdisplay (_fun _GdkDisplay -> _Display)
  #:make-fail make-not-available)

(define-gdk gdk_x11_visual_get_xvisual (_fun _GdkVisual -> _Visual)
  #:make-fail make-not-available)
