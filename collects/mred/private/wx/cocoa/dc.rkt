#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/unsafe/cairo
         racket/draw/private/bitmap
         racket/draw/private/local
         racket/draw/private/gl-context
         "types.rkt"
         "utils.rkt"
         "bitmap.rkt"
         "window.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/backing-dc.rkt"
         "cg.rkt")

(provide 
 (protect-out dc%
              do-backing-flush))

(import-class NSOpenGLContext)

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

    (define/override (flush)
      (send canvas flush))

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
