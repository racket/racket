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

(define-gdi32 GetClipBox (_wfun _pointer _RECT-pointer -> _int))
(define SIMPLEREGION 2)

(define-gdi32 BitBlt (_wfun _pointer _int _int _int _int _pointer _int _int _DWORD -> _BOOL))
(define SRCCOPY #X00cc0020)

(define hwnd-param (make-parameter #f))

(define need-clip-text-workaround? #t)
(define need-clip-refresh-workaround? #f) ; patched Cairo

(define win32-bitmap%
  (class win32-no-hwnd-bitmap%
    (init w h hwnd [gl-config #f])
    (inherit get-cairo-surface)
    (parameterize ([hwnd-param hwnd])
      (super-new [w w] [h h] [backing-scale (->screen 1.0)]))
    
    (define/override (build-cairo-surface w h backing-scale)
      (define hwnd (hwnd-param))
      (if hwnd
          (atomically
           (let ([hdc (GetDC hwnd)])
             (begin0
	      (let ([sw (inexact->exact (floor (* backing-scale w)))]
		    [sh (inexact->exact (floor (* backing-scale h)))])
		(cairo_win32_surface_create_with_ddb hdc
						     CAIRO_FORMAT_RGB24 sw sh))
               (ReleaseDC hwnd hdc))))
          (super build-cairo-surface w h backing-scale)))
    
    (define gl (and gl-config
                    (let ([hdc (cairo_win32_surface_get_dc (get-cairo-surface))])
                      (set-cpointer-tag! hdc 'HDC)
                      (create-gl-context hdc 
                                         gl-config
                                         #t))))
    (define/override (get-bitmap-gl-context) gl)))

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
      ;; until we draw more text --- but only with DDB surfaces.
      (when need-clip-text-workaround?
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
	  (make-object bitmap% w h #f #t (->screen 1.0))))

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
	    (define sw (->screen (unbox w)))
	    (define sh (->screen (unbox h)))
	    (define r (make-RECT 0 0 sw sh))
	    (define clip-type
	      (if need-clip-refresh-workaround?
		  (GetClipBox hdc r)
		  SIMPLEREGION))
	    (cond
	     [(and need-clip-refresh-workaround?
		   (not (and (= clip-type SIMPLEREGION)
			     (= (RECT-left r) 0)
			     (= (RECT-top r) 0)
			     (= (RECT-right r) sw)
			     (= (RECT-bottom r) sh))))
	      ;; Another workaround: a clipping region installed by BeginPaint()
	      ;; seems to interfere with Cairo drawing. So, draw to a
	      ;; fresh context and copy back and forth using Win32.
	      (define cw (- (RECT-right r) (RECT-left r)))
	      (define ch (- (RECT-bottom r) (RECT-top r)))
	      (let* ([surface (cairo_win32_surface_create_with_ddb hdc
								   CAIRO_FORMAT_RGB24
								   cw
								   ch)]
		     [cr (cairo_create surface)]
		     [hdc2 (cairo_win32_surface_get_dc surface)])
		(BitBlt hdc2 0 0 cw ch hdc (RECT-left r) (RECT-top r) SRCCOPY)
		(cairo_scale cr (->screen 1.0) (->screen 1.0))
		(backing-draw-bm bm cr (->normal sw) (->normal sh)
				 (->normal (- (RECT-left r))) (->normal (- (RECT-top r)))
				 (->screen 1.0))
		(cairo_surface_flush surface)
		(BitBlt hdc (RECT-left r) (RECT-top r) cw ch hdc2 0 0 SRCCOPY)
		(cairo_surface_destroy surface)
		(cairo_destroy cr))]
	     [else
	      (let* ([surface (cairo_win32_surface_create hdc)]
		     [cr (cairo_create surface)])
		(cairo_surface_destroy surface)
		(cairo_scale cr (->screen 1.0) (->screen 1.0))
		(backing-draw-bm bm cr (->normal sw) (->normal sh)
				 0 0
				 (->screen 1.0))
		(cairo_destroy cr))])))))

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
