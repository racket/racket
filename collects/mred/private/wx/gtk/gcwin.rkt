#lang racket/base
(require ffi/unsafe
         "utils.rkt"
         "types.rkt"
         "window.rkt")

(provide 
 (protect-out scheme_add_gc_callback
              scheme_remove_gc_callback
              create-gc-window
              make-gc-show-desc
              make-gc-hide-desc))

(define-cstruct _GdkWindowAttr
  ([title _string]
   [event_mask _int]
   [x _int]
   [y _int]
   [width _int]
   [height _int]
   [wclass _int] ; GDK_INPUT_OUTPUT
   [visual _pointer]
   [colormap _pointer]
   [window_type _int] ; GDK_WINDOW_CHILD
   [cursor _pointer]
   [wmclass_name _string]
   [wmclass_class _string]
   [override_redirect _gboolean]
   [type_hint _int]))

(define << arithmetic-shift)

(define GDK_WA_TITLE (1 . << . 1))
(define GDK_WA_X (1 . << . 2))
(define GDK_WA_Y (1 . << . 3))
(define GDK_WA_CURSOR (1 . << . 4))
(define GDK_WA_COLORMAP (1 . << . 5))
(define GDK_WA_VISUAL (1 . << . 6))
(define GDK_WA_WMCLASS (1 . << . 7))
(define GDK_WA_NOREDIR (1 . << . 8))
(define GDK_WA_TYPE_HINT (1 . << . 9))

(define GDK_INPUT_OUTPUT 0)

(define GDK_WINDOW_CHILD 2)

(define-gdk gdk_window_new (_fun _GdkWindow _GdkWindowAttr-pointer _uint
                                 -> _GdkWindow))

(define-gdk gdk_window_show _fpointer)
(define-gdk gdk_window_hide _fpointer)
(define-gdk gdk_display_flush _fpointer)
(define-gdk gdk_draw_pixbuf _fpointer)

(define-mz scheme_add_gc_callback (_fun _racket _racket -> _racket))
(define-mz scheme_remove_gc_callback (_fun _racket -> _void))

(define (create-gc-window cwin x y w h)
  (let ([win (gdk_window_new cwin (make-GdkWindowAttr
                                   ""
                                   0
                                   x y w h
                                   GDK_INPUT_OUTPUT
                                   #f #f 
                                   GDK_WINDOW_CHILD
                                   #f
                                   "" "" #f 0)
                             (bitwise-ior GDK_WA_X
                                          GDK_WA_Y))])
    win))

(define (make-draw win pixbuf w h)
  (vector 'ptr_ptr_ptr_int_int_int_int_int_int_int_int_int->void
          gdk_draw_pixbuf
          win #f pixbuf
          0 0 0 0 w h
          0 0 0))

(define (make-flush)
  (vector 'ptr_ptr_ptr->void gdk_display_flush (gdk_display_get_default) #f #f))

(define (make-gc-show-desc win pixbuf w h)
  (vector
   (vector 'ptr_ptr_ptr->void gdk_window_show win #f #f)
   (make-draw win pixbuf w h)
   (make-flush)))

(define (make-gc-hide-desc win pixbuf w h)
  (vector
   ;; draw the ``off'' bitmap so we can flush immediately
   (make-draw win pixbuf w h)
   (make-flush)
   ;; hide the window; it may take a while for the underlying canvas
   ;; to refresh:
   (vector 'ptr_ptr_ptr->void gdk_window_hide win #f #f)))
