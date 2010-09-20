#lang racket/base
(require ffi/unsafe
	 racket/class
	 "utils.rkt"
	 "types.rkt"
	 "const.rkt"
	 "icons.rkt")

(provide hInstance
	 DefWindowProcW
	 win32->wx
	 set-win32-wx!
	 MessageBoxW)

;; ----------------------------------------

(define-user32 GetWindowLongW (_wfun _HWND _int -> _pointer))
(define-user32 SetWindowLongW (_wfun _HWND _int _pointer -> _pointer))

(define (win32->wx win32)
  (let ([p (GetWindowLongW win32 GWLP_USERDATA)])
    (and p (ptr-ref p _racket))))

(define (set-win32-wx! win32 wx)
  (SetWindowLongW win32 GWLP_USERDATA (malloc-immobile-cell wx)))

;; ----------------------------------------

(define-cstruct _INITCOMMONCONTROLSEX
  ([dwSize _DWORD]
   [dwICC _DWORD]))

(define-comctl32 InitCommonControlsEx (_wfun _INITCOMMONCONTROLSEX-pointer -> _BOOL))

(void
 (InitCommonControlsEx (make-INITCOMMONCONTROLSEX
			(ctype-sizeof _INITCOMMONCONTROLSEX)
			0)))

;; ----------------------------------------

(define _WndProc (_wfun #:atomic? #t #:keep (box null)
			_HWND _UINT _WPARAM _LPARAM -> _LRESULT))

(define-cstruct _WNDCLASS ([style _UINT]
			   [lpfnWndProc _WndProc]
			   [cbClsExtra _int]
			   [cbWndExtra _int]
			   [hInstace _HINSTANCE]
			   [hIcon _HICON]
			   [hCursor _HCURSOR]
			   [hbrBackground _HBRUSH]
			   [lpszMenuName _permanent-string/utf-16]
			   [lpszClassName _permanent-string/utf-16]))

(define-user32 RegisterClassW (_wfun _WNDCLASS-pointer -> _ATOM))
(define-kernel32 GetModuleHandleW (_wfun _pointer -> _HINSTANCE))
(define-user32 LoadCursorW (_wfun _HINSTANCE _pointer -> _HCURSOR))
(define-user32 LoadIconW (_wfun _HINSTANCE _pointer -> _HICON))

(define-user32 DefWindowProcW (_wfun _HWND _UINT _WPARAM _LPARAM -> _LRESULT))

#;(define-user32 PostQuitMessage (_wfun _int -> _void))

(define (wind-proc w msg wparam lparam)
  (let ([wx (win32->wx w)])
    (if wx
        (send wx wndproc w msg wparam lparam)
        (DefWindowProcW w msg wparam lparam))))

(define hInstance (GetModuleHandleW #f))

(void (RegisterClassW (make-WNDCLASS CS_OWNDC
				     wind-proc
				     0
                                     0
				     hInstance
				     (LoadIconW #f IDI_APPLICATION)
				     (LoadCursorW #f IDC_ARROW)
                                     (let ([p (ptr-add #f (+ COLOR_BTNFACE 1))])
                                       (cpointer-push-tag! p 'HBRUSH)
                                       p)
				     #f ; menu
				     "PLTFrame")))

(void (RegisterClassW (make-WNDCLASS 0 ; not CS_OWNDC !
				     wind-proc
				     0
                                     0
				     hInstance
				     #f
				     (LoadCursorW #f IDC_ARROW)
                                     (let ([p (ptr-add #f (+ COLOR_WINDOW 1))])
                                       (cpointer-push-tag! p 'HBRUSH)
                                       p)
				     #f ; menu
				     "PLTCanvas")))

(void (RegisterClassW (make-WNDCLASS 0
				     wind-proc
				     0
                                     0
				     hInstance
				     #f
				     (LoadCursorW #f IDC_ARROW)
                                     (let ([p (ptr-add #f (+ COLOR_BTNFACE 1))])
                                       (cpointer-push-tag! p 'HBRUSH)
                                       p)
				     #f ; menu
				     "PLTPanel")))

(define-user32 MessageBoxW (_fun _HWND _string/utf-16 _string/utf-16 _UINT -> _int))
