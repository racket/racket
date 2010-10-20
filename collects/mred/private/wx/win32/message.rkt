#lang racket/base
(require racket/class
         racket/draw
         racket/promise
         ffi/unsafe
          "../../syntax.rkt"
          "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "hbitmap.rkt"
         "types.rkt")

(provide message%)

(define STM_SETIMAGE        #x0172)

(define SS_LEFT #x00000000)
(define SS_BITMAP #x0000000E)
(define SS_ICON #x00000003)

(define IDI_APPLICATION     32512)
(define IDI_HAND            32513)
(define IDI_QUESTION        32514)
(define IDI_EXCLAMATION     32515)
(define IDI_WARNING         IDI_EXCLAMATION)
(define IDI_ERROR           IDI_HAND)

(define IMAGE_ICON 1)

(define-user32 LoadIconW (_wfun _HINSTANCE _LONG -> _HICON))
(define-kernel32 GetModuleFileNameW (_wfun _pointer _pointer _DWORD -> _DWORD))

(define-shell32 ExtractIconW (_wfun _HINSTANCE _string/utf-16 _UINT -> (r : _HICON)
                                    -> (or r (failed 'ExtractIconW))))
                                           
(define ERROR_INSUFFICIENT_BUFFER 122)

(define app-icon
  (delay
    (let ()
      (let ([path
             (let loop ([size 1024])
               (let ([p (make-bytes (* (ctype-sizeof _WCHAR) 1024))])
                 (let ([r (GetModuleFileNameW #f p size)])
                   (cond
                    [(and (or (zero? r) (= r size))
                          (= (GetLastError) ERROR_INSUFFICIENT_BUFFER))
                     (loop (* size 2))]
                    [(zero? r) (failed 'GetModuleFileNameW)]
                    [else (cast p _gcpointer _string/utf-16)]))))])
        (if path
            (ExtractIconW hInstance path 0)
            (LoadIconW #f IDI_APPLICATION))))))
(define warning-icon
  (delay
    (LoadIconW #f IDI_WARNING)))
(define error-icon
  (delay
    (LoadIconW #f IDI_ERROR)))

(define message%
  (class item%
    (inherit auto-size set-size set-control-font get-hwnd
             subclass-control
             remember-label-bitmap)

    (init parent label
          x y
          style font)

    (define bitmap?
      (and (label . is-a? . bitmap%)
           (send label ok?)))

    (define/public (get-class) "PLTSTATIC")
    
    (super-new [callback void]
               [parent parent]
               [hwnd 
                (CreateWindowExW 0
                                 (get-class)
                                 (if (string? label)
                                     label
                                     "<image>")
                                 (bitwise-ior SS_LEFT WS_CHILD WS_CLIPSIBLINGS
                                              (if bitmap?
                                                  SS_BITMAP
                                                  (if (symbol? label)
                                                      SS_ICON
                                                      0)))
                                 0 0 0 0
                                 (send parent get-client-hwnd)
                                 #f
                                 hInstance
                                 #f)]
               [style style])

    (subclass-control (get-hwnd))

    (when bitmap?
      (let ([hbitmap (bitmap->hbitmap label)])
        (remember-label-bitmap hbitmap)
        (SendMessageW (get-hwnd) STM_SETIMAGE IMAGE_BITMAP 
                      (cast hbitmap _HBITMAP _LPARAM))))

    (when (symbol? label)
      (SendMessageW (get-hwnd) STM_SETIMAGE IMAGE_ICON
                    (cast (force (case label
                                   [(caution) warning-icon]
                                   [(stop) error-icon]
                                   [else app-icon]))
                          _HICON _LPARAM)))
    
    (set-control-font font)

    (if (symbol? label)
        (set-size -11111 -11111 32 32)
        (auto-size font label 0 0 0 0))

    (define/override (get-setimage-message)
      STM_SETIMAGE)))
