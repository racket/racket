#lang racket/base
(require ffi/unsafe
         racket/class
         racket/string
         racket/draw/private/color
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt"
	 "../../lock.rkt")

(provide 
 (protect-out get-color-from-user))

(define-cstruct _CHOOSECOLOR
  ([lStructSize _DWORD]
   [hwndOwner _HWND]
   [hInstance _HWND]
   [rgbResult _COLORREF]
   [lpCustColors _pointer]
   [Flags _DWORD]
   [lCustData _LPARAM]
   [lpfnHook _fpointer]
   [lpTemplateName _fpointer]))

(define CC_RGBINIT #x00000001)

(define-comdlg32 ChooseColorW (_wfun _CHOOSECOLOR-pointer -> _BOOL))

(define custom-colors (malloc 'raw 16 _COLORREF))
(memset custom-colors 255 16 _COLORREF)

(define (get-color-from-user message parent color)
  (atomically
   (let ([p (malloc 'raw _CHOOSECOLOR)])
     (memset p 0 1 _CHOOSECOLOR)
     (set-cpointer-tag! p CHOOSECOLOR-tag)
     (set-CHOOSECOLOR-lStructSize! p (ctype-sizeof _CHOOSECOLOR))
     (when parent
       (set-CHOOSECOLOR-hwndOwner! p (send parent get-hwnd)))
     (when color
       (set-CHOOSECOLOR-rgbResult! p (make-COLORREF
                                      (color-red color)
                                      (color-green color)
                                      (color-blue color)))
       (set-CHOOSECOLOR-Flags! p CC_RGBINIT))
     (set-CHOOSECOLOR-lpCustColors! p custom-colors)
     (begin0
      (and (ChooseColorW p)
           (let ([c (CHOOSECOLOR-rgbResult p)])
             (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))
      (free p)))))

