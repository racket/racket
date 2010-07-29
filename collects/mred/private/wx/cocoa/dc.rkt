#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
         racket/draw/cairo
         racket/draw/dc
         racket/draw/local
         "../common/queue.rkt"
         "../../syntax.rkt")

(provide dc%
         _CGContextRef
         CGContextSetRGBFillColor
         CGContextFillRect)

(define _CGContextRef (_cpointer 'CGContextRef))
(define-appserv CGContextTranslateCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextScaleCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextFlush (_fun _CGContextRef -> _void))
(define-appserv CGContextSetRGBFillColor (_fun _CGContextRef _CGFloat _CGFloat _CGFloat _CGFloat -> _void))
(define-appserv CGContextFillRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextConvertPointToUserSpace (_fun  _CGContextRef _NSPoint -> _NSPoint))
(define-appserv CGContextConvertSizeToUserSpace (_fun  _CGContextRef _NSSize -> _NSSize))

(define dc-backend%
  (class* default-dc-backend% (dc-backend<%>)
    (init context dx dy width height)
    (super-new)

    (inherit reset-cr)

    (define the-context context) ;; retain as long as we need `cg'
    (define cg (tell #:type _CGContextRef context graphicsPort))

    (define old-dx 0)
    (define old-dy 0)

    (define/private (set-bounds dx dy width height)
      (set! old-dx dx)
      (set! old-dy (+ dy height))
      (CGContextTranslateCTM cg old-dx old-dy)
      (CGContextScaleCTM cg 1 -1)
      (let ([surface (cairo_quartz_surface_create_for_cg_context cg width height)])
        (set! cr (cairo_create surface))
        (cairo_surface_destroy surface))
      (set! clip-width width)
      (set! clip-height height)
      (cairo_rectangle cr 0 0 width height)
      (cairo_clip cr))

    (define clip-width width)
    (define clip-height height)

    (define/override (reset-clip cr)
      (super reset-clip cr)
      (cairo_rectangle cr 0 0 clip-width clip-height)
      (cairo_clip cr))

    (define cr #f)
    (set-bounds dx dy width height)

    (define/public (reset-bounds dx dy width height)
      (let ([old-cr cr])
        (when old-cr
          (set! cr #f)
          (cairo_destroy old-cr)))
      (CGContextScaleCTM cg 1 -1)
      (CGContextTranslateCTM cg (- old-dx) (- old-dy))
      (set-bounds dx dy width height)
      (reset-cr cr))

    (def/override (get-size)
      (values (exact->inexact clip-width)
              (exact->inexact clip-height)))

    (define/override (get-cr) cr)
    
    (define/override (flush-cr)
      (add-event-boundary-sometimes-callback! cg CGContextFlush))))

(define dc%
  (dc-mixin dc-backend%))

