#lang racket/base
(require ffi/unsafe
	 ffi/unsafe/define
	 ffi/unsafe/alloc
         "../common/utils.rkt"
         "types.rkt")

(provide
 define-mz
 (protect-out define-gdi32
              define-user32
              define-kernel32
              define-comctl32
              define-comdlg32
              define-shell32
              define-uxtheme
              define-winmm
              failed
	      is-win64?

              GetLastError

              GetWindowLongPtrW
              SetWindowLongPtrW
              SendMessageW SendMessageW/str SendMessageW/ptr
              GetSysColor GetRValue GetGValue GetBValue make-COLORREF
              CreateBitmap
              CreateCompatibleBitmap
              DeleteObject
              CreateCompatibleDC
              DeleteDC
              MoveWindow
              ShowWindow
              EnableWindow
              SetWindowTextW
              SetCursor
              GetDC
              ReleaseDC
              InvalidateRect
              ValidateRect
              GetMenuState
              CheckMenuItem
              ModifyMenuW
              RemoveMenu
              SelectObject
              WideCharToMultiByte))

(define gdi32-lib (ffi-lib "gdi32.dll"))
(define user32-lib (ffi-lib "user32.dll"))
(define kernel32-lib (ffi-lib "kernel32.dll"))
(define comctl32-lib (ffi-lib "comctl32.dll"))
(define comdlg32-lib (ffi-lib "comdlg32.dll"))
(define shell32-lib (ffi-lib "shell32.dll"))
(define uxtheme-lib (ffi-lib "uxtheme.dll"))
(define winmm-lib (ffi-lib "winmm.dll"))

(define-ffi-definer define-gdi32 gdi32-lib)
(define-ffi-definer define-user32 user32-lib)
(define-ffi-definer define-kernel32 kernel32-lib)
(define-ffi-definer define-comctl32 comctl32-lib)
(define-ffi-definer define-comdlg32 comdlg32-lib)
(define-ffi-definer define-shell32 shell32-lib)
(define-ffi-definer define-uxtheme uxtheme-lib)
(define-ffi-definer define-winmm winmm-lib)

(define-kernel32 GetLastError (_wfun -> _DWORD))

(define (failed who)
  ;; There's a race condition between this use of GetLastError()
  ;;  and other Racket threads that may have run since
  ;;  the call in this thread that we're reporting as failed.
  ;;  In the rare case that we lose a race, though, it just
  ;;  means a bad report for an error that shouldn't have happened
  ;;; anyway.
  (error who "call failed (~s)"
         (GetLastError)))

(define is-win64?
  (equal? "win32\\x86_64" 
	  (path->string (system-library-subpath #f))))

(define GetWindowLongPtrW
  (get-ffi-obj (if is-win64? 'GetWindowLongPtrW 'GetWindowLongW) user32-lib
	       (_wfun _HWND _int -> _pointer)))
(define SetWindowLongPtrW
  (get-ffi-obj (if is-win64? 'SetWindowLongPtrW 'SetWindowLongW) user32-lib
	       (_wfun _HWND _int _pointer -> _pointer)))

(define-user32 SendMessageW (_wfun _HWND _UINT _WPARAM _LPARAM -> _LRESULT))
(define-user32 SendMessageW/str (_wfun _HWND _UINT _WPARAM _string/utf-16 -> _LRESULT)
  #:c-id SendMessageW)
(define-user32 SendMessageW/ptr (_wfun _HWND _UINT _WPARAM _pointer -> _LRESULT)
  #:c-id SendMessageW)

(define-user32 GetSysColor (_wfun _int -> _DWORD))

(define (GetRValue v) (bitwise-and v #xFF))
(define (GetGValue v) (bitwise-and (arithmetic-shift v -8) #xFF))
(define (GetBValue v) (bitwise-and (arithmetic-shift v -16) #xFF))
(define (make-COLORREF r g b) (bitwise-ior
                               r
                               (arithmetic-shift g 8)
                               (arithmetic-shift b 16)))

(define-user32 MoveWindow(_wfun _HWND _int _int _int _int _BOOL -> (r : _BOOL)
                                -> (unless r (failed 'MoveWindow))))

(define-user32 ShowWindow (_wfun _HWND _int -> (previously-shown? : _BOOL) -> (void)))
(define-user32 EnableWindow (_wfun _HWND _BOOL -> _BOOL))

(define-user32 SetWindowTextW (_wfun _HWND _string/utf-16 -> (r : _BOOL)
                                     -> (unless r (failed 'SetWindowText))))

(define-user32 SetCursor (_wfun _HCURSOR -> _HCURSOR))

(define-user32 _GetDC (_wfun  _HWND -> _HDC)
  #:c-id GetDC)
(define (GetDC hwnd)
  (((allocator (lambda (hdc) (ReleaseDC hwnd hdc)))
    _GetDC)
   hwnd))

(define-user32 ReleaseDC (_wfun _HWND _HDC -> _int)
  #:wrap (deallocator cadr))

(define-gdi32 DeleteObject (_wfun _pointer -> (r : _BOOL)
                                  -> (unless r (failed 'DeleteObject)))
  #:wrap (deallocator))

(define-gdi32 CreateCompatibleBitmap (_wfun _HDC _int _int -> _HBITMAP)
  #:wrap (allocator DeleteObject))
(define-gdi32 CreateBitmap (_wfun _int _int _UINT _UINT _pointer -> _HBITMAP)
  #:wrap (allocator DeleteObject))

(define-gdi32 DeleteDC (_wfun _HDC -> (r : _BOOL)
                              -> (unless r (failed 'DeleteDC)))
  #:wrap (deallocator))
(define-gdi32 CreateCompatibleDC (_wfun _HDC -> _HDC)
  #:wrap (allocator DeleteDC))

(define-user32 InvalidateRect (_wfun _HWND (_or-null _RECT-pointer) _BOOL -> (r : _BOOL)
                                     -> (unless r (failed 'InvalidateRect))))
(define-user32 ValidateRect (_wfun _HWND (_or-null _RECT-pointer) -> (r : _BOOL)
                                   -> (unless r (failed 'ValidateRect))))

(define-user32 GetMenuState (_wfun _HMENU _UINT _UINT -> _UINT))
(define-user32 CheckMenuItem (_wfun _HMENU _UINT _UINT -> _DWORD))
(define-user32 ModifyMenuW (_wfun _HMENU _UINT _UINT _UINT_PTR _string/utf-16
                                  -> (r : _BOOL)
                                  -> (unless r (failed 'ModifyMenuW))))
(define-user32 RemoveMenu (_wfun _HMENU _UINT _UINT -> (r : _BOOL)
                                 -> (unless r (failed 'RemoveMenu))))

(define-gdi32 SelectObject (_wfun _HDC _pointer -> _pointer))

(define-kernel32 WideCharToMultiByte (_wfun _UINT _DWORD _pointer _int
                                            _pointer _int _pointer _pointer
                                            -> _int))
