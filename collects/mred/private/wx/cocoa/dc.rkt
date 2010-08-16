#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
         racket/draw/cairo
         racket/draw/local
         "types.rkt"
         "utils.rkt"
         "window.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/backing-dc.rkt")

(provide dc%
         do-backing-flush

         _CGContextRef
         CGContextSetRGBFillColor
         CGContextFillRect
         CGContextAddRect
         CGContextStrokePath
         CGContextAddLines)

(define _CGContextRef (_cpointer 'CGContextRef))
(define-appserv CGContextSynchronize (_fun _CGContextRef -> _void))
(define-appserv CGContextTranslateCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextScaleCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextSaveGState (_fun _CGContextRef -> _void))
(define-appserv CGContextRestoreGState (_fun _CGContextRef -> _void))
(define-appserv CGContextSetRGBFillColor (_fun _CGContextRef _CGFloat _CGFloat _CGFloat _CGFloat -> _void))
(define-appserv CGContextFillRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextAddRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextAddLines (_fun _CGContextRef (v : (_vector i _NSPoint)) (_long = (vector-length v)) -> _void))
(define-appserv CGContextStrokePath (_fun _CGContextRef -> _void))

(define quartz-bitmap%
  (class object%
    (init w h b&w? alpha?)
    (super-new)
    (define s
      (cairo_quartz_surface_create CAIRO_FORMAT_ARGB32
                                   w
                                   h))

    (define/public (ok?) #t)
    (define/public (is-color?) #t)

    (define width w)
    (define height h)
    (define/public (get-width) width)
    (define/public (get-height) height)
    
    (define/public (get-cairo-surface) s)

    (define/public (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (set! s #f)))))

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)])
    (define canvas cnvs)

    (super-new)

    ;; Use a quartz bitmap so that text looks good:
    (define/override (get-bitmap%) quartz-bitmap%)
    (define/override (can-combine-text? sz) #t)

    (define/override (get-backing-size xb yb)
      (send canvas get-backing-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      ;; called atomically (not expecting exceptions)
      (send canvas queue-backing-flush))

    (define suspend-count 0)
    (define req #f)

    (define/override (suspend-flush) 
      (atomically
       (when (zero? suspend-count)
         (set! req (request-flush-delay (send canvas get-cocoa-window))))
       (set! suspend-count (add1 suspend-count))
       (super suspend-flush)))

    (define/override (resume-flush) 
      (atomically
       (set! suspend-count (sub1 suspend-count))
       (when (and (zero? suspend-count) req)
         (cancel-flush-delay req)
         (set! req #f))
       (super resume-flush)))))

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
                 (cairo_destroy cr))))))
   (tellv ctx restoreGraphicsState)))
