#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
         "../common/utils.rkt"
         "types.rkt")

(provide define-gdi32
	 define-user32
	 define-kernel32
	 define-comctl32
	 define-uxtheme
         define-mz
         failed

         SendMessageW)

(define gdi32-lib (ffi-lib "gdi32.dll"))
(define user32-lib (ffi-lib "user32.dll"))
(define kernel32-lib (ffi-lib "kernel32.dll"))
(define comctl32-lib (ffi-lib "comctl32.dll"))
(define uxtheme-lib (ffi-lib "uxtheme.dll"))

(define-ffi-definer define-gdi32 gdi32-lib)
(define-ffi-definer define-user32 user32-lib)
(define-ffi-definer define-kernel32 kernel32-lib)
(define-ffi-definer define-comctl32 comctl32-lib)
(define-ffi-definer define-uxtheme uxtheme-lib)

(define-kernel32 GetLastError (_wfun -> _DWORD))

(define (failed w who)
  (error who "call failed (~s)"
         (GetLastError)))

(define-user32 SendMessageW (_wfun _HWND _UINT _WPARAM _LPARAM -> _LRESULT))

