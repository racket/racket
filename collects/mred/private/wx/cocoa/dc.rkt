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
         "window.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/backing-dc.rkt"
         "cg.rkt")

(provide 
 (protect-out dc%
              do-backing-flush))

(import-class NSOpenGLContext)
(define NSOpenGLCPSwapInterval 222)

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)]
          transparent?)
    (define canvas cnvs)

    (inherit end-delay)
    (super-new [transparent? transparent?])

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
                   ;; Disable screen sync for GL flushBuffer; otherwise,
                   ;; flushBuffer can take around 10 msec depending on the timing
                   ;; of event polling, and that can be bad for examples like gears.
                   ;; Maybe whether to sync with the screen should be a configuration
                   ;; option, but I can't tell the difference on my screen.
                   (tellv gl-ctx setValues: 
                          #:type (_ptr i _long) 0
                          forParameter: #:type _int NSOpenGLCPSwapInterval)
                   (set! gl g)
                   g)))))

    ;; Use a quartz bitmap so that text looks good:
    (define trans? transparent?)
    (define/override (make-backing-bitmap w h) 
      (make-object quartz-bitmap% w h trans?))
    (define/override (can-combine-text? sz) #t)

    (define/override (get-backing-size xb yb)
      (send canvas get-backing-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      ;; Re-enable expose events so that the queued 
      ;; backing flush will be handled:
      (end-delay)
      (send canvas queue-backing-flush))

    (define/override (flush)
      (send canvas flush))

    (define/override (request-delay)
      (send canvas request-canvas-flush-delay))
    (define/override (cancel-delay req)
      (send canvas cancel-canvas-flush-delay req))))

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
                 (backing-draw-bm bm cr (unbox w) (unbox h))
                 (cairo_destroy cr))))))
   (tellv ctx restoreGraphicsState)))
