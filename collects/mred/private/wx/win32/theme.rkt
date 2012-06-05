#lang racket/base
(require ffi/unsafe
         ffi/unsafe/alloc
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "font.rkt")

(provide
 (protect-out get-theme-logfont
              get-theme-font-face
              get-theme-font-size
              OpenThemeData
              CloseThemeData
              DrawThemeParentBackground
              DrawThemeBackground
              DrawThemeEdge
              EnableThemeDialogTexture))

(define _HTHEME (_cpointer 'HTHEME))

(define-uxtheme CloseThemeData (_wfun _HTHEME -> (r : _HRESULT)
				      -> (when (negative? r)
					   (error 'CloseThemeData "failed: ~s" (bitwise-and #xFFFF r))))
  #:wrap (deallocator))
(define (maybe-CloseThemeData v) (when v (CloseThemeData v)))
(define-uxtheme OpenThemeData (_wfun _HWND _string/utf-16 -> (_or-null _HTHEME))
  #:wrap (allocator maybe-CloseThemeData))

(define-uxtheme GetThemeFont (_wfun _HTHEME _HDC _int _int _int (f : (_ptr o _LOGFONTW))
				    -> (r : _HRESULT)
				    -> (if (negative? r) 
					   (error 'GetThemeFont "failed: ~s" (bitwise-and #xFFFF r))
					   f)))

(define-uxtheme GetThemeSysFont(_wfun (_or-null _HTHEME) _int (f : (_ptr o _LOGFONTW))
				      -> (r : _HRESULT)
				      -> (if (negative? r) 
					     (error 'GetThemeSysFont "failed: ~s" (bitwise-and #xFFFF r))
					     f)))

(define-uxtheme DrawThemeBackground (_wfun _HTHEME _HDC _int _int _RECT-pointer (_or-null _RECT-pointer) -> (r : _HRESULT)
                                           -> (when (negative? r)
                                                (error 'DrawThemeBackground "failed: ~s" (bitwise-and #xFFFF r)))))
(define-uxtheme DrawThemeParentBackground (_wfun _HWND _HDC _pointer -> (r : _HRESULT)
                                                 -> (when (negative? r)
                                                      (error 'DrawThemeParentBackground "failed: ~s" (bitwise-and #xFFFF r)))))
(define-uxtheme DrawThemeEdge (_wfun _HWND _HDC _int _int _RECT-pointer _int _int _RECT-pointer -> (r : _HRESULT)
                                     -> (when (negative? r)
                                          (error 'DrawThemeEdge "failed: ~s" (bitwise-and #xFFFF r)))))

(define-uxtheme EnableThemeDialogTexture (_wfun _HWND _DWORD -> (r : _HRESULT)
                                                -> (when (negative? r)
                                                     (error 'EnableThemeDialogTexture "failed: ~s" (bitwise-and #xFFFF r)))))

(define BP_PUSHBUTTON 1)
(define PBS_NORMAL 1)
(define TMT_FONT 210)
(define TMT_BODYFONT 809)

(define TMT_MSGBOXFONT 805)

(define theme-logfont (GetThemeSysFont #f TMT_MSGBOXFONT))

(define (get-theme-logfont)
  theme-logfont)

(define (get-theme-font-face)
  (cast (array-ptr (LOGFONTW-lfFaceName theme-logfont)) _pointer _string/utf-16))

(define (get-theme-font-size)
  (abs (LOGFONTW-lfHeight theme-logfont)))
