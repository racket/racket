#lang racket/base
(require ffi/unsafe
         racket/draw/unsafe/cairo
         racket/draw
         racket/draw/private/local
         racket/class
	 "dc.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt")

(provide
 (protect-out bitmap->hbitmap
              hbitmap->bitmap))

(define (bitmap->hbitmap bm 
                         #:mask [mask-bm #f]
                         #:b&w? [b&w? #f] 
                         #:bg [bg (GetSysColor COLOR_BTNFACE)])
  (let* ([w (send bm get-width)]
         [h (send bm get-height)]
	 [bm (if (bm . is-a? . win32-bitmap%)
		 ;; Windows wants to use the result bitmap
		 ;; as an ARGB bitmap, but Cairo seems to transfer
		 ;; RGB win32 bitmaps to RGB win32 bitmaps in a
		 ;; way that sometimes mangles the alpha; avoid the
		 ;; problem by first copying to a Cairo memory bitmap.
		 (let* ([new-b (make-object bitmap% w h #f #f (send bm get-backing-scale))]
			[dc (make-object bitmap-dc% new-b)])
		   (send dc draw-bitmap bm 0 0)
		   (send dc set-bitmap #f)
		   new-b)
		 bm)]
         [mask-bm (or mask-bm
                      (send bm get-loaded-mask))]
         [to-frac (lambda (v) (/ v 255.0))]
         [screen-hdc (GetDC #f)]
         [hdc (CreateCompatibleDC screen-hdc)]
	 [sc (->screen 1.0)]
	 [scaled (lambda (v) (inexact->exact (ceiling (* v sc))))]
         [hbitmap (if b&w?
                      (CreateBitmap w h 1 1 #f)
                      (CreateCompatibleBitmap screen-hdc (scaled w) (scaled h)))]
         [old-hbitmap (SelectObject hdc hbitmap)])
    (ReleaseDC #f screen-hdc)
    (let* ([s (cairo_win32_surface_create hdc)]
           [cr (cairo_create s)])
      (cairo_surface_destroy s)
      (unless (= sc 1)
	(cairo_scale cr sc sc))
      (cairo_set_source_rgba cr 
                             (to-frac (GetRValue bg))
                             (to-frac (GetGValue bg))
                             (to-frac (GetBValue bg))
                             1.0)
      (cairo_paint cr)
      (let ([mask-p (and mask-bm
                         (cairo_pattern_create_for_surface 
                          (send mask-bm get-cairo-alpha-surface)))])
        (let ([p (cairo_get_source cr)])
          (cairo_pattern_reference p)
          (cairo_set_source_surface cr (send bm get-cairo-surface) 0 0)
          (let ([sc (send bm get-cairo-device-scale)])
            (unless (= sc 1)
              (let ([m (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0)])
                (cairo_matrix_init_translate m 0 0)
                (cairo_matrix_scale m sc sc)
                (cairo_pattern_set_matrix (cairo_get_source cr) m))))
          (if mask-p
              (cairo_mask cr mask-p)
              (begin
                (cairo_new_path cr)
                (cairo_rectangle cr 0 0 w h)
                (cairo_fill cr)))
          (when mask-p
            (cairo_pattern_destroy mask-p))
          (cairo_set_source cr p)
          (cairo_pattern_destroy p)))
      (cairo_destroy cr)
      (SelectObject hdc old-hbitmap)
      (DeleteDC hdc)
      hbitmap)))

(define-cstruct _BITMAP
  ([bmType _LONG]
   [bmWidth _LONG]
   [bmHeight _LONG]
   [bmWidthBytes _LONG]
   [bmPlanes _WORD]
   [bmBitsPixel _WORD]
   [bmBits _pointer]))

(define-gdi32 GetObjectW (_wfun _pointer _int _pointer -> (r : _int)
                                -> (when (zero? r) (failed 'GetObject))))

(define (hbitmap->bitmap hbitmap)
  (let* ([bmi (let ([b (make-BITMAP 0 0 0 0 0 0 #f)])
                (GetObjectW hbitmap (ctype-sizeof _BITMAP) b)
                b)]
         [w (BITMAP-bmWidth bmi)]
         [h (BITMAP-bmHeight bmi)]
         [screen-hdc (GetDC #f)]
         [hdc (CreateCompatibleDC screen-hdc)]
         [old-hbitmap (SelectObject hdc hbitmap)]
         [bm (make-object bitmap% w h (= 1 (BITMAP-bmBitsPixel bmi)) #t)])
    (ReleaseDC #f screen-hdc)
    (let* ([s (cairo_win32_surface_create hdc)]
           [cr (cairo_create (send bm get-cairo-surface))])
      (let ([p (cairo_get_source cr)])
        (cairo_pattern_reference p)
        (cairo_set_source_surface cr s 0 0)
        (cairo_new_path cr)
        (cairo_rectangle cr 0 0 w h)
        (cairo_fill cr)
        (cairo_set_source cr p)
        (cairo_pattern_destroy p))
      (cairo_destroy cr)
      (SelectObject hdc old-hbitmap)
      (DeleteDC hdc)
      bm)))
