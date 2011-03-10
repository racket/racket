#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/unsafe/cairo
         racket/draw/private/bitmap
         racket/draw/private/local
         "types.rkt"
         "utils.rkt"
         "../../lock.rkt"
         "cg.rkt")

(provide quartz-bitmap%)

(define quartz-bitmap%
  (class bitmap%
    (init w h [with-alpha? #t])
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s
      (let ([s (cairo_quartz_surface_create (if with-alpha?
                                                CAIRO_FORMAT_ARGB32
                                                CAIRO_FORMAT_RGB24)
                                            w
                                            h)])
        ;; initialize bitmap to empty - needed?
        (let ([cr (cairo_create s)])
          (cairo_set_operator cr (if with-alpha?
                                     CAIRO_OPERATOR_CLEAR
                                     CAIRO_OPERATOR_SOURCE))
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_paint cr)
          (cairo_destroy cr))
        s))

    (define/override (ok?) (and s #t))

    (define/override (is-color?) #t)

    (define has-alpha? with-alpha?)
    (define/override (has-alpha-channel?) has-alpha?)

    (define/override (get-cairo-surface) s)
    (define/override (get-cairo-alpha-surface) 
      (if has-alpha?
          s
          (super get-cairo-alpha-surface)))

    (define/override (release-bitmap-storage)
      (atomically
       (when s
         (cairo_surface_destroy s)
         (set! s #f))))))
