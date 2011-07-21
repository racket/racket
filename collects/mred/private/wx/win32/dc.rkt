#lang racket/base
(require ffi/unsafe
	 ffi/winapi
         racket/class
         "utils.rkt"
         "types.rkt"
         "gl-context.rkt"
	 "../../lock.rkt"
         "../common/backing-dc.rkt"
         "../common/delay.rkt"
         racket/draw/unsafe/cairo
         racket/draw/private/dc
         racket/draw/private/bitmap
         racket/draw/private/local
         ffi/unsafe/alloc)

(provide 
 (protect-out dc%
              win32-bitmap%
              do-backing-flush
              request-flush-delay
              cancel-flush-delay))

(define-gdi32 SelectClipRgn (_wfun _pointer _pointer -> _int))

(define win32-bitmap%
  (class bitmap%
    (init w h hwnd [gl-config #f])
    (super-make-object (make-alternate-bitmap-kind w h))

    (define s
      (let ([s
             (if (not hwnd)
                 (cairo_win32_surface_create_with_dib CAIRO_FORMAT_RGB24 w h)
                 (atomically
                  (let ([hdc (GetDC hwnd)])
                    (begin0
                     (cairo_win32_surface_create_with_ddb hdc
                                                          CAIRO_FORMAT_RGB24 w h)
                     (ReleaseDC hwnd hdc)))))])
        ;; initialize bitmap to white:
        (let ([cr (cairo_create s)])
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_paint cr)
          (cairo_destroy cr))
        s))
    
    (define gl (and gl-config
                    (let ([hdc (cairo_win32_surface_get_dc s)])
                      (set-cpointer-tag! hdc 'HDC)
                      (create-gl-context hdc 
                                         gl-config
                                         #t))))
    (define/override (get-bitmap-gl-context) gl)

    (define/override (ok?) #t)
    (define/override (is-color?) #t)
    (define/override (has-alpha-channel?) #f)

    (define/override (get-cairo-surface) s)

    (define/override (release-bitmap-storage)
      (atomically
       (cairo_surface_destroy s)
       (set! s #f)))))

(define dc%
  (class backing-dc%
    (init [(cnvs canvas)]
          transparent?)
    (inherit end-delay)
    (define canvas cnvs)

    (super-new [transparent? transparent?])

    (inherit internal-get-bitmap)
    (define/override (reset-clip cr)
      (super reset-clip cr)
      ;; Work around a Cairo(?) bug. When a clipping
      ;; region is set, we draw text, and then the clipping
      ;; region is changed, the change doesn't take
      ;; until we draw more text --- but only under Win64,
      ;; and only with DDB surfaces.
      (when win64?
	(let ([bm (internal-get-bitmap)])
	  (when (bm . is-a? . win32-bitmap%)
	    (SelectClipRgn (cairo_win32_surface_get_dc
			    (send bm get-cairo-surface))
			   #f)))))

    (define gl #f)
    (define/override (get-gl-context)
      (or gl
          (let ([v (create-gl-context (GetDC (send canvas get-client-hwnd))
                                      (send canvas get-gl-config)
                                      #f)])
	    (when v (set! gl v))
	    v)))
      

    (define/override (make-backing-bitmap w h)
      (if (send canvas get-canvas-background)
          (make-object win32-bitmap% w h (send canvas get-hwnd))
          (super make-backing-bitmap w h)))

    (define/override (get-backing-size xb yb)
      (send canvas get-client-size xb yb))

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
      (request-flush-delay canvas))
    (define/override (cancel-delay req)
      (cancel-flush-delay req))))

(define (do-backing-flush canvas dc hdc)
  (send dc on-backing-flush
        (lambda (bm)
          (let ([w (box 0)]
                [h (box 0)])
            (send canvas get-client-size w h)
            (let* ([surface (cairo_win32_surface_create hdc)]
                   [cr (cairo_create surface)])
              (cairo_surface_destroy surface)
              (backing-draw-bm bm cr (unbox w) (unbox h))
              (cairo_destroy cr))))))

(define (request-flush-delay canvas)
  (do-request-flush-delay 
   canvas
   (lambda (gtk)
     (send canvas suspend-paint-handling))
   (lambda (gtk)
     (send canvas resume-paint-handling))))

(define (cancel-flush-delay req)
  (when req
    (do-cancel-flush-delay 
     req
     (lambda (canvas)
       (send canvas resume-paint-handling)))))
