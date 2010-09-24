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
(define-gdi32 CreateCompatibleDC (_wfun _HDC -> _HDC))
(define-gdi32 DeleteDC (_wfun _HDC -> (r : _BOOL)
                              -> (unless r (failed 'DeleteDC))))
(define-gdi32 SelectObject (_wfun _HDC _HBITMAP -> _HBITMAP))
(define-user32 GetDC (_wfun  _HWND -> _HDC))
(define-user32 ReleaseDC (_wfun _HWND _HDC -> _int))

(define (bitmap->hbitmap bm)
  (let* ([w (send bm get-width)]
         [h (send bm get-height)]
         [col (GetSysColor COLOR_BTNFACE)]
         [to-frac (lambda (v) (/ v 255.0))]
         [screen-hdc (GetDC #f)]
         [hdc (CreateCompatibleDC screen-hdc)]
         [hbitmap (CreateCompatibleBitmap screen-hdc w h)]
         [old-hbitmap (SelectObject hdc hbitmap)])
    (ReleaseDC #f screen-hdc)
    (let* ([s (cairo_win32_surface_create hdc)]
           [cr (cairo_create s)])
      (cairo_surface_destroy s)
      (cairo_set_source_rgba cr 
                             (to-frac (GetRValue col))
                             (to-frac (GetGValue col))
                             (to-frac (GetBValue col))
                             1.0)
      (cairo_paint cr)
      (let ([p (cairo_get_source cr)])
        (cairo_pattern_reference p)
        (cairo_set_source_surface cr (send bm get-cairo-surface) 0 0)
        (cairo_new_path cr)
        (cairo_rectangle cr 0 0 w h)
        (cairo_fill cr)
        (cairo_set_source cr p)
        (cairo_pattern_destroy p))
      (cairo_destroy cr)
      (SelectObject hdc old-hbitmap)
      (DeleteDC hdc)
      hbitmap)))


