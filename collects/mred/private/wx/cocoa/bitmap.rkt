#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/cairo
         racket/draw/bitmap
         racket/draw/local
         "types.rkt"
         "utils.rkt"
         "../../lock.rkt"
         "cg.rkt")

(provide quartz-bitmap%)

(define quartz-bitmap%
  (class bitmap%
    (init w h)
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s
      (let ([s (cairo_quartz_surface_create CAIRO_FORMAT_ARGB32
                                            w
                                            h)])
        ;; initialize bitmap to empty - needed?
        #;
        (let ([cr (cairo_create s)])
          (cairo_set_operator cr CAIRO_OPERATOR_CLEAR)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_paint cr)
          (cairo_destroy cr))
        s))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)

    (define/override (get-cairo-surface) s)
    (define/override (get-cairo-alpha-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (when s
         (cairo_surface_destroy s)
         (set! s #f))))))