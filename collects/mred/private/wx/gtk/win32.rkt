#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         "utils.rkt")

(provide gdk_win32_drawable_get_handle
	 GetDC
	 ReleaseDC)

(define user32-lib
  (cond
   [(eq? 'windows (system-type))
    (ffi-lib "user32.dll")]
   [else #f]))

(define-ffi-definer define-user32 user32-lib)

(define _GdkDrawable _pointer)

(define-gdk gdk_win32_drawable_get_handle (_fun _GdkDrawable -> _pointer)
  #:make-fail make-not-available)

(define-user32 GetDC (_fun #:abi 'stdcall _pointer -> _pointer)
  #:make-fail make-not-available)
(define-user32 ReleaseDC (_fun #:abi 'stdcall _pointer -> _void)
  #:make-fail make-not-available)
