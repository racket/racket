#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/cairo
         racket/draw/bitmap
         racket/draw/local
         racket/draw/gl-context
         "types.rkt"
         "utils.rkt"
         "window.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/backing-dc.rkt"
         "cg.rkt")

(provide dc%
         quartz-bitmap%
         do-backing-flush)

(import-class NSOpenGLContext)

(define quartz-bitmap%
  (class bitmap%
    (init w h)
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s
      (cairo_quartz_surface_create CAIRO_FORMAT_ARGB32
                                   w
                                   h))

    (define/override (ok?) #t)
    (define/override (is-color?) #t)

    (define/override (get-cairo-surface) s)
    (define/override (get-cairo-alpha-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (when s
         (cairo_surface_destroy s)
         (set! s #f))))))

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)])
    (define canvas cnvs)

    (super-new)

    (define gl #f)
    (define/override (get-gl-context)
      (and (send canvas can-gl?)
           (let ([gl-ctx (tell (send canvas get-cocoa-content) openGLContext)])
             (or gl
                 (let ([g (new (class gl-context%
                                 (define/override (do-call-as-current t)
                                   (dynamic-wind
                                       (lambda () (tellv gl-ctx makeCurrentContext))
                                       t
                                       (lambda () (tellv NSOpenGLContext clearCurrentContext))))
                                 (define/override (do-swap-buffers)
                                   (tellv gl-ctx flushBuffer))
                                 (super-new)))])
                   (set! gl g)
                   g)))))

    ;; Use a quartz bitmap so that text looks good:
    (define/override (make-backing-bitmap w h) (make-object quartz-bitmap% w h))
    (define/override (can-combine-text? sz) #t)

    (define/override (get-backing-size xb yb)
      (send canvas get-backing-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      ;; With Cocoa window-level delay doesn't stop
      ;; displays; it blocks flushes to the screen.
      ;; So leave the delay in place, and `end-delay'
      ;; after displaying to the window (after which
      ;; we'll be ready to flush the window), which
      ;; is at then end of `do-backing-flush'.
      (send canvas queue-backing-flush))

    (define/override (request-delay)
      (request-flush-delay (send canvas get-flush-window)))
    (define/override (cancel-delay req)
      (cancel-flush-delay req))))

(define (do-backing-flush canvas dc ctx dx dy)
  (tellv ctx saveGraphicsState)
  (begin0
   (send dc on-backing-flush
         (lambda (bm)
           (let ([w (box 0)]
                 [h (box 0)])
             (send canvas get-client-size w h)
             (let ([cg (tell #:type _CGContextRef ctx graphicsPort)])
               (unless (send canvas is-flipped?)
                 (CGContextTranslateCTM cg 0 (unbox h))
                 (CGContextScaleCTM cg 1 -1))
               (CGContextTranslateCTM cg dx dy)
               (let* ([surface (cairo_quartz_surface_create_for_cg_context cg (unbox w) (unbox h))]
                      [cr (cairo_create surface)])
                 (cairo_surface_destroy surface)
                 (let ([s (cairo_get_source cr)])
                   (cairo_pattern_reference s)
                   (cairo_set_source_surface cr (send bm get-cairo-surface) 0 0)
                   (cairo_new_path cr)
                   (cairo_rectangle cr 0 0 (unbox w) (unbox h))
                   (cairo_fill cr)
                   (cairo_set_source cr s)
                   (cairo_pattern_destroy s))
                 (cairo_destroy cr))))
           (send dc end-delay)))
   (tellv ctx restoreGraphicsState)))
