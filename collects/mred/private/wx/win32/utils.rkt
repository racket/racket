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

         GetWindowLongW
         SetWindowLongW
         SendMessageW SendMessageW/str
         GetSysColor GetRValue GetGValue GetBValue
         MoveWindow
         ShowWindow
         EnableWindow
         SetWindowTextW
         SetCursor
         GetDC
         ReleaseDC
         InvalidateRect
         GetMenuState
         CheckMenuItem
         ModifyMenuW
         RemoveMenu)

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

(define (failed who)
  (error who "call failed (~s)"
         (GetLastError)))

(define-user32 GetWindowLongW (_wfun _HWND _int -> _pointer))
(define-user32 SetWindowLongW (_wfun _HWND _int _pointer -> _pointer))

(define-user32 SendMessageW (_wfun _HWND _UINT _WPARAM _LPARAM -> _LRESULT))
(define-user32 SendMessageW/str (_wfun _HWND _UINT _WPARAM _string/utf-16 -> _LRESULT)
  #:c-id SendMessageW)

(define-user32 GetSysColor (_wfun _int -> _DWORD))

(define (GetRValue v) (bitwise-and v #xFF))
(define (GetGValue v) (bitwise-and (arithmetic-shift v -8) #xFF))
(define (GetBValue v) (bitwise-and (arithmetic-shift v -16) #xFF))

(define-user32 MoveWindow(_wfun _HWND _int _int _int _int _BOOL -> (r : _BOOL)
                                -> (unless r (failed 'MoveWindow))))

(define-user32 ShowWindow (_wfun _HWND _int -> (previously-shown? : _BOOL) -> (void)))
(define-user32 EnableWindow (_wfun _HWND _BOOL -> _BOOL))

(define-user32 SetWindowTextW (_wfun _HWND _string/utf-16 -> (r : _BOOL)
                                     -> (unless r (failed 'SetWindowText))))

(define-user32 SetCursor (_wfun _HCURSOR -> _HCURSOR))

(define-user32 GetDC (_wfun  _HWND -> _HDC))
(define-user32 ReleaseDC (_wfun _HWND _HDC -> _int))

(define-user32 InvalidateRect (_wfun _HWND (_or-null _RECT-pointer) _BOOL -> (r : _BOOL)
                                     -> (unless r (failed 'InvalidateRect))))

(define-user32 GetMenuState (_wfun _HMENU _UINT _UINT -> _UINT))
(define-user32 CheckMenuItem (_wfun _HMENU _UINT _UINT -> _DWORD))
(define-user32 ModifyMenuW (_wfun _HMENU _UINT _UINT _UINT_PTR _string/utf-16
                                  -> (r : _BOOL)
                                  -> (unless r (failed 'ModifyMenuW))))
(define-user32 RemoveMenu (_wfun _HMENU _UINT _UINT -> (r : _BOOL)
                                 -> (unless r (failed 'RemoveMenu))))
