#lang racket/base
(require ffi/unsafe)

(provide _wfun
	 
	 _DWORD 	
	 _ATOM
	 _WPARAM
	 _LPARAM
	 _LRESULT
	 _BOOL
	 _UINT
	 _BYTE
	 _LONG
	 _SHORT
	 _HRESULT
         _WCHAR

	 _HINSTANCE
	 _HWND
	 _HMENU
	 _HICON
	 _HCURSOR
	 _HBRUSH
	 _HDC
	 _HFONT
	 _HBITMAP

	 _COLORREF

	 _fnpointer

	 _permanent-string/utf-16

	 (struct-out POINT) _POINT _POINT-pointer 
	 (struct-out RECT) _RECT _RECT-pointer
	 (struct-out MSG) _MSG _MSG-pointer

         HIWORD
         LOWORD
         MAKELONG
         MAKELPARAM)

(define-syntax-rule (_wfun . a)
  (_fun #:abi 'stdcall . a))

(define _DWORD _int32)
(define _ATOM _int)
(define _WPARAM _long)
(define _LPARAM _long)
(define _LRESULT _long)
(define _BOOL (make-ctype _int (lambda (v) (if v 1 0)) (lambda (v) (not (zero? v)))))
(define _UINT _uint)
(define _BYTE _uint8)
(define _HRESULT _int32)
(define _WCHAR _int16)

(define _HINSTANCE (_cpointer/null 'HINSTANCE))
(define _HWND (_cpointer/null 'HWND))
(define _HMENU (_cpointer/null 'HMENU))
(define _HICON (_cpointer/null 'HICON))
(define _HCURSOR (_cpointer/null 'HCURSOR))
(define _HBRUSH (_cpointer/null 'HBRUSH))
(define _HDC (_cpointer/null 'HDC))
(define _HFONT (_cpointer/null 'HFONT))
(define _HBITMAP (_cpointer/null 'HBITMAP))

(define _COLORREF _DWORD)

(define _fnpointer (_or-null _fpointer))

(define _permanent-string/utf-16
  (make-ctype _pointer
	      (lambda (s)
		(and s
		     (let ([v (malloc _gcpointer)])
		       (ptr-set! v _string/utf-16 s)
		       (let ([p (ptr-ref v _gcpointer)])
			 (let ([len (let loop ([i 0])
				      (if (zero? (ptr-ref p _uint16 i))
					  (add1 i)
					  (loop (add1 i))))])
			   (let ([c (malloc len _uint16 'raw)])
			     (memcpy c p len _uint16)
			     c))))))
	      (lambda (p)
		(and p
		     (cast p _pointer _string/utf-16)))))

(define _LONG _long)
(define _SHORT _short)

(define-cstruct _POINT ([x _LONG]
			[y _LONG]))

(define-cstruct _RECT ([left _LONG]
		       [top _LONG]
		       [right _LONG]
		       [bottom _LONG]))

(define-cstruct _MSG ([hwnd _HWND]
		      [message _UINT]
		      [wParam _WPARAM]
		      [lParam _LPARAM]
		      [time _DWORD]
		      [pt _POINT]))

(define (HIWORD v)
  (arithmetic-shift v -16))
(define (LOWORD v)
  (bitwise-and v #xFFFF))

(define (MAKELONG a b)
  (bitwise-ior (arithmetic-shift b 16) a))
(define (MAKELPARAM a b) (MAKELONG a b))
