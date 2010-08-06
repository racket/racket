#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
	 "../../lock.rkt"
         "../common/backing-dc.rkt"
         racket/draw/cairo
         racket/draw/dc
         racket/draw/local
         ffi/unsafe/alloc)

(provide dc%
         do-backing-flush)

(define-gdk gdk_cairo_create (_fun _pointer -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)])
    (define canvas cnvs)

    (super-new)

    (define/override (get-backing-size xb yb)
      (send canvas get-client-size xb yb))

    (define/override (get-size)
      (let ([xb (box 0)]
            [yb (box 0)])
        (send canvas get-virtual-size xb yb)
        (values (unbox xb) (unbox yb))))

    (define/override (queue-backing-flush)
      (send canvas queue-backing-flush))

    (define suspend-count 0)
    (define req #f)

    (define/override (suspend-flush) 
      (as-entry
       (lambda ()
         #;
         (when (zero? suspend-count)
           (set! req (request-flush-delay (send canvas get-cocoa-window))))
         (set! suspend-count (add1 suspend-count))
         (super suspend-flush))))

    (define/override (resume-flush) 
      (as-entry
       (lambda ()
         (set! suspend-count (sub1 suspend-count))
         #;
         (when (and (zero? suspend-count) req)
           (cancel-flush-delay req)
           (set! req #f))
         (super resume-flush))))))

(define (do-backing-flush canvas dc win)
  (send dc on-backing-flush
        (lambda (bm)
          (let ([w (box 0)]
                [h (box 0)])
            (send canvas get-client-size w h)
            (let ([cr (gdk_cairo_create win)])
              (let ([s (cairo_get_source cr)])
                (cairo_pattern_reference s)
                (cairo_set_source_surface cr (send bm get-cairo-surface) 0 0)
                (cairo_new_path cr)
                (cairo_rectangle cr 0 0 (unbox w) (unbox h))
                (cairo_fill cr)
                (cairo_set_source cr s)
                (cairo_pattern_destroy s))
              (cairo_destroy cr))))))
