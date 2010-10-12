#lang scheme/base
(require ffi/unsafe
         racket/draw/cairo
         racket/draw
         racket/draw/local
         racket/class
         "types.rkt"
         "utils.rkt"
         "const.rkt")

(provide bitmap->hbitmap)

(define-gdi32 CreateCompatibleBitmap (_wfun _HDC _int _int -> _HBITMAP))
(define-gdi32 CreateBitmap (_wfun _int _int _UINT _UINT _pointer -> _HBITMAP))
(define-gdi32 CreateCompatibleDC (_wfun _HDC -> _HDC))
(define-gdi32 DeleteDC (_wfun _HDC -> (r : _BOOL)
                              -> (unless r (failed 'DeleteDC))))

(define (bitmap->hbitmap bm 
                         #:mask [mask-bm #f]
                         #:b&w? [b&w? #f] 
                         #:bg [bg (GetSysColor COLOR_BTNFACE)])
  (let* ([w (send bm get-width)]
         [h (send bm get-height)]
         [mask-bm (or mask-bm
                      (send bm get-loaded-mask))]
         [to-frac (lambda (v) (/ v 255.0))]
         [screen-hdc (GetDC #f)]
         [hdc (CreateCompatibleDC screen-hdc)]
         [hbitmap (if b&w?
                      (CreateBitmap w h 1 1 #f)
                      (CreateCompatibleBitmap screen-hdc w h))]
         [old-hbitmap (SelectObject hdc hbitmap)])
    (ReleaseDC #f screen-hdc)
    (let* ([s (cairo_win32_surface_create hdc)]
           [cr (cairo_create s)])
      (cairo_surface_destroy s)
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


