#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
	 "../../lock.rkt"
         racket/draw/cairo
         racket/draw/dc
         racket/draw/local
         ffi/unsafe/alloc)

(provide dc% reset-dc-size)

(define-gdk gdk_cairo_create (_fun _pointer -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define-local-member-name
  reset-dc-size)

(define dc-backend%
  (class default-dc-backend%
    (init-field gtk
                get-client-size)

    (define c #f)

    (define/override (get-cr)
      (or c
          (let ([w (g_object_get_window gtk)])
            (and w
                 (set! c (gdk_cairo_create w))
                 c))))

    (define/public (reset-dc-size)
      (when (eq? 'windows (system-type))
        ;; FIXME: ensure that the dc is not in use
        (as-entry
         (lambda ()
	   (when c
	     (cairo_destroy c)
	     (set! c #f))))))

    (define/override (get-size)
      (let-values ([(w h) (get-client-size)])
      (values (exact->inexact w)
              (exact->inexact h))))
    
    (super-new)))

(define dc%
  (dc-mixin dc-backend%))
