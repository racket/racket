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

(provide dc% reset-dc)

(define-gdk gdk_cairo_create (_fun _pointer -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define-local-member-name
  reset-dc)

(define dc-backend%
  (class default-dc-backend%
    (init-field gtk
                get-client-size
		window-lock
                [get-window g_object_get_window])
    (inherit reset-cr set-auto-scroll)

    (define c #f)

    (define/override (get-cr)
      (or c
          (let ([w (get-window gtk)])
            (and w
		 (begin
		   ;; Under Windows, creating a Cairo context within
		   ;; a frame inteferes with any other Cairo context 
		   ;; within the same frame. So we use a lock to 
		   ;; serialize drawing to different contexts.
		   (when window-lock (semaphore-wait window-lock))
                   (set! c (gdk_cairo_create w))
		   (reset-cr c)
		   c)))))

    (define/override (release-cr cr)
      (when window-lock
	(cairo_destroy c)
	(set! c #f)
	(semaphore-post window-lock)))

    (define/public (reset-dc scroll-dx scroll-dy)
      ;; FIXME: ensure that the dc is not in use
      (as-entry
       (lambda ()
         (when c
           (cairo_destroy c)
           (set! c #f))
         (set-auto-scroll scroll-dx scroll-dy))))

    (define/override (get-size)
      (let-values ([(w h) (get-client-size)])
      (values (exact->inexact w)
              (exact->inexact h))))
    
    (super-new)))

(define dc%
  (dc-mixin dc-backend%))
