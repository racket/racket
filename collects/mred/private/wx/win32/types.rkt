#lang racket/base
(require ffi/unsafe
         ffi/winapi)

(provide
 (protect-out _wfun
              
              _WORD
              _DWORD
              _UDWORD
              _ATOM
              _WPARAM
              _LPARAM
              _LRESULT
              _BOOL
              _UINT
              _UINT_PTR
              _BYTE
              _LONG
              _ULONG
              _SHORT
              _HRESULT
              _WCHAR
              _SIZE_T
              _INT_PTR

              _HINSTANCE
              _HWND
              _HMENU
              _HICON
              _HCURSOR
              _HBRUSH
              _HDC
              _HFONT
              _HBITMAP
              _HANDLE

              _COLORREF

              _fnpointer

              _permanent-string/utf-16
              utf-16-length

              (struct-out POINT) _POINT _POINT-pointer 
              (struct-out RECT) _RECT _RECT-pointer
              (struct-out MSG) _MSG _MSG-pointer

              HIWORD
              LOWORD
              MAKELONG
              MAKELPARAM))

(define-syntax-rule (_wfun . a)
  (_fun #:abi winapi . a))

(define _WORD _int16)
(define _DWORD _int32)
(define _UDWORD _uint32)
(define _ATOM _int)
(define _UINT_PTR _uintptr)
(define _WPARAM _intptr) ; supposed to be _UINT_PTR, but we have some sign mismatch
(define _LONG_PTR _intptr)
(define _LPARAM _LONG_PTR)
(define _LRESULT _LONG_PTR)
(define _BOOL (make-ctype _int (lambda (v) (if v 1 0)) (lambda (v) (not (zero? v)))))
(define _UINT _uint)
(define _BYTE _uint8)
(define _HRESULT _long)
(define _WCHAR _int16)
(define _SIZE_T _long)
(define _INT_PTR _intptr)

(define _HINSTANCE (_cpointer/null 'HINSTANCE))
(define _HWND (_cpointer/null 'HWND))
(define _HMENU (_cpointer/null 'HMENU))
(define _HICON (_cpointer/null 'HICON))
(define _HCURSOR (_cpointer/null 'HCURSOR))
(define _HBRUSH (_cpointer/null 'HBRUSH))
(define _HDC (_cpointer/null 'HDC))
(define _HFONT (_cpointer/null 'HFONT))
(define _HBITMAP (_cpointer/null 'HBITMAP))
(define _HANDLE (_cpointer/null 'HANDLE))

(define _COLORREF _DWORD)

(define _fnpointer (_or-null _fpointer))

(define (utf-16-length s)
  (for/fold ([len 0]) ([c (in-string s)])
    (+ len
       (if ((char->integer c) . > . #xFFFF)
           2
           1))))

(define _permanent-string/utf-16
  (make-ctype _pointer
              (lambda (s)
                (and s
                     (let ([v (malloc _gcpointer)])
                       (ptr-set! v _string/utf-16 s)
                       (let ([p (ptr-ref v _gcpointer)])
                         (let ([len (+ 1 (utf-16-length s))])
                           (let ([c (malloc len _uint16 'raw)])
                             (memcpy c p len _uint16)
                             c))))))
              (lambda (p) p)))

(define _LONG _long)
(define _ULONG _ulong)
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

(define (short v)
  (if (zero? (bitwise-and #x8000 v))
      v
      (bitwise-ior v (arithmetic-shift -1 15))))

(define (HIWORD v)
  (short (arithmetic-shift v -16)))
(define (LOWORD v)
  (short (bitwise-and v #xFFFF)))

(define (MAKELONG a b)
  (bitwise-ior (arithmetic-shift b 16) 
               (bitwise-and a #xFFFF)))
(define (MAKELPARAM a b) (MAKELONG a b))
