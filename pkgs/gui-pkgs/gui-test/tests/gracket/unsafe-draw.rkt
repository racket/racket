#lang racket/base
(require ffi/unsafe
         racket/draw/unsafe/cairo-lib
         racket/draw/unsafe/brush)

(provide surface-brush)

(define cairo_image_surface_create
  (get-ffi-obj 'cairo_image_surface_create cairo-lib (_fun _int _int _int -> _pointer)))
(define cairo_surface_destroy
  (get-ffi-obj 'cairo_surface_destroy cairo-lib (_fun _pointer -> _void)))
(define cairo_create
  (get-ffi-obj 'cairo_create cairo-lib (_fun _pointer -> _pointer)))
(define cairo_destroy
  (get-ffi-obj 'cairo_destroy cairo-lib (_fun _pointer -> _void)))

(define cairo_set_source_rgba
  (get-ffi-obj 'cairo_set_source_rgba cairo-lib (_fun _pointer _double* _double* _double* _double* -> _void)))
(define cairo_rectangle
  (get-ffi-obj 'cairo_rectangle cairo-lib (_fun _pointer _double* _double* _double* _double* -> _void)))
(define cairo_fill
  (get-ffi-obj 'cairo_fill cairo-lib (_fun _pointer -> _void)))

(define s (cairo_image_surface_create 0 20 30))
(define cr (cairo_create s))
(cairo_set_source_rgba cr 1.0 0.0 0.0 0.5)
(cairo_rectangle cr 2 2 16 26)
(cairo_fill cr)
(cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
(cairo_rectangle cr 9 9 2 2)
(cairo_fill cr)
(cairo_destroy cr)

(define surface-brush (make-handle-brush s 20 30 '#(#(1 0 0 1 420 320) 0 0 1 1 0)))

(cairo_surface_destroy s)
