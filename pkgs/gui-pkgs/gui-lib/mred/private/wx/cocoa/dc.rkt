#lang racket/base
(require "../../syntax.rkt"
         racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/unsafe/cairo
         racket/draw/private/bitmap
         racket/draw/private/local
         racket/draw/private/gl-context
         "types.rkt"
         "utils.rkt"
         "const.rkt"
         "window.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/backing-dc.rkt"
         "cg.rkt")

(provide 
 (protect-out dc%
              do-backing-flush)
 display-bitmap-resolution
 make-screen-bitmap
 make-window-bitmap)

(import-class NSOpenGLContext NSScreen NSGraphicsContext NSWindow)

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
                                 (define/override (get-handle) gl-ctx)
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
      (make-window-bitmap w h (send canvas get-cocoa-window) 
                          trans?
                          (send canvas is-flipped?)))
                   
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

(define-local-member-name get-layer)

(define (do-backing-flush canvas dc ctx dx dy)
  (tellv ctx saveGraphicsState)
  (begin0
   (send dc on-backing-flush
         (lambda (bm)
           (let ([w (box 0)]
                 [h (box 0)])
             (send canvas get-client-size w h)
             (let ([cg (tell #:type _CGContextRef ctx graphicsPort)])
               (cond
                [(bm . is-a? . layer-bitmap%)
                 (define layer (send bm get-layer))
                 (CGContextDrawLayerAtPoint cg (make-NSPoint dx dy) layer)]
                [else
                 (unless (send canvas is-flipped?)
                   (CGContextTranslateCTM cg 0 (unbox h))
                   (CGContextScaleCTM cg 1 -1))
                 (CGContextTranslateCTM cg dx dy)
                 (let* ([surface (cairo_quartz_surface_create_for_cg_context cg (unbox w) (unbox h))]
                        [cr (cairo_create surface)])
                   (cairo_surface_destroy surface)
                   (backing-draw-bm bm cr (unbox w) (unbox h))
                   (cairo_destroy cr))])))))
   (tellv ctx restoreGraphicsState)))

(define (display-bitmap-resolution num fail)
  (if (version-10.7-or-later?)
      (let ([r (atomically
                (with-autorelease
                 (let ([s (if (zero? num)
                              (tell NSScreen mainScreen)
                              (let ([screens (tell NSScreen screens)])
                                (if (num . < . (tell #:type _NSUInteger screens count))
                                    (tell screens objectAtIndex: #:type _NSUInteger num)
                                    #f)))])
                   (and s
                        (tell #:type _CGFloat s backingScaleFactor)))))])
        (cond
         [(not r) (fail)]
         [(zero? r) 1.0]
         [else r]))
      1.0))

(define/top (make-screen-bitmap [exact-positive-integer? w]
                                [exact-positive-integer? h])
  (make-object quartz-bitmap% w h #t
               (display-bitmap-resolution 0 void)))

(define (make-window-bitmap w h win [trans? #t] [flipped? #f])
  (if win
      (make-object layer-bitmap% w h win trans? flipped?)
      (make-screen-bitmap w h)))

(define layer-bitmap%
  (class quartz-bitmap%
    (init w h win trans? flipped?)

    (define layer (make-layer win w h))
    (define layer-w w)
    (define layer-h h)
    (define/public (get-layer) layer)

    (define is-trans? trans?)

    (super-make-object w h trans? 1
                       (let ([cg (CGLayerGetContext layer)])
                         (unless flipped?
                           (CGContextTranslateCTM cg 0 h)
                           (CGContextScaleCTM cg 1 -1))
                         cg))

    (define/override (draw-bitmap-to cr sx sy dx dy w h alpha clipping-region)
      ;; Called when the destination rectangle is inside the clipping region
      (define s (cairo_get_target cr))
      (cond
       [(and (= (cairo_surface_get_type s) CAIRO_SURFACE_TYPE_QUARTZ)
             (= sx 0)
             (= sy 0)
             (let ([rs (cairo_copy_clip_rectangle_list cr)])
               (cond
                [(and (= CAIRO_STATUS_SUCCESS (cairo_rectangle_list_t-status rs))
                      (< (cairo_rectangle_list_t-num_rectangles rs) 64))
                 rs]
                [else
                 (cairo_rectangle_list_destroy rs)
                 #f])))
        =>
        (lambda (rs)
          ;; Use fast layer drawing:
          (unless (or (zero? (cairo_rectangle_list_t-num_rectangles rs))
                      (zero? alpha))
            (atomically
             (define m (make-cairo_matrix_t 0 0 0 0 0 0))
             (cairo_get_matrix cr m)
             (define trans
               (make-CGAffineTransform (cairo_matrix_t-xx m)
                                       (cairo_matrix_t-yx m)
                                       (cairo_matrix_t-xy m)
                                       (cairo_matrix_t-yy m)
                                       (cairo_matrix_t-x0 m)
                                       (cairo_matrix_t-y0 m)))
             (cairo_surface_flush s)
             (define cg (cairo_quartz_surface_get_cg_context s))
             (CGContextSaveGState cg)
             (CGContextConcatCTM cg trans)
             (let ([n (cairo_rectangle_list_t-num_rectangles rs)])
               (define vec
                 (for/vector #:length n ([i (in-range n)])
                   (define r (ptr-add (cairo_rectangle_list_t-rectangles rs) i _cairo_rectangle_t))
                   (make-NSRect (make-NSPoint (cairo_rectangle_t-x r)
                                              (cairo_rectangle_t-y r))
                                (make-NSSize (cairo_rectangle_t-width r)
                                             (cairo_rectangle_t-height r)))))
               (CGContextClipToRects cg vec n))
             ;; Flip target, because drawing to layer was flipped
             (CGContextTranslateCTM cg 0 (+ dy h))
             (CGContextScaleCTM cg 1 -1)
             (CGContextSetAlpha cg alpha)
             (CGContextDrawLayerInRect cg
                                       (make-NSRect (make-NSPoint dx 0)
                                                    (make-NSSize w h))
                                       layer)
             
             (CGContextRestoreGState cg)
             (cairo_surface_mark_dirty s)))
          (cairo_rectangle_list_destroy rs)
          #t)]
       [else #f]))

    (define s-bm #f)
    (define/override (get-cairo-surface)
      ;; Convert to a platform bitmap, which Cairo understands
      (let ([t-bm (or s-bm
                      (let ([bm (make-object quartz-bitmap%
                                             layer-w layer-h 
                                             is-trans?
                                             1.0)])
                        (define dc (send bm make-dc))
                        ;; For some reason, we must touch the DC
                        ;; to make transarent work right. It works
                        ;; to draw beyond the visible region:
                        (send dc draw-rectangle (+ layer-w 5) (+ layer-h 5) 1 1)
                        (send dc draw-bitmap this 0 0)
                        (send dc set-bitmap #f)
                        (set! s-bm bm)
                        bm))])
        (send t-bm get-cairo-surface)))

    (define/override (get-cairo-target-surface)
      (super get-cairo-surface))

    (define/override (drop-alpha-s)
      (super drop-alpha-s)
      (set! s-bm #f))

    (define/override (release-bitmap-storage)
      (super release-bitmap-storage)
      (set! s-bm #f)
      (atomically
       (when layer
         (CGLayerRelease layer)
         (set! layer #f))))))

(define (make-layer win w h)
  (atomically
   (with-autorelease
    (let* ([ctx (if ((tell #:type _NSInteger win windowNumber) . <= . 0)
                    ;; Window's device is gone, probably because it was hidden,
                    ;; and we configure windows with setOneShot: YES.
                    ;; Just make a new window...
                    (let ([alt-win (make-temp-window)])
                      (begin0
                       (tell NSGraphicsContext graphicsContextWithWindow: alt-win)
                       (tellv alt-win release)))
                    ;; Use canvas's window:
                    (tell NSGraphicsContext graphicsContextWithWindow: win))]
           [tmp-cg (tell #:type _CGContextRef ctx graphicsPort)]
           [layer (CGLayerCreateWithContext tmp-cg (make-NSSize w h) #f)])
      layer))))

(define (make-temp-window)
  (tell (tell NSWindow alloc)
        initWithContentRect: #:type _NSRect (make-NSRect (make-NSPoint 0 0)
                                                         (make-NSSize 10 10))
        styleMask: #:type _int 0
        backing: #:type _int NSBackingStoreBuffered
        defer: #:type _BOOL NO))
