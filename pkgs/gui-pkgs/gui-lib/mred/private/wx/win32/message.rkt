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
         "types.rkt"
	 "icons.rkt")

(provide
 (protect-out message%))

(define STM_SETIMAGE        #x0172)

(define SS_LEFT #x00000000)
(define SS_LEFTNOWORDWRAP #x0000000C)
(define SS_BITMAP #x0000000E)
(define SS_ICON #x00000003)

(define IMAGE_ICON 1)

(define warning-icon
  (delay
    (LoadIconW #f IDI_WARNING)))
(define error-icon
  (delay
    (LoadIconW #f IDI_ERROR)))

(define message%
  (class item%
    (inherit auto-size set-size set-control-font get-hwnd
             remember-label-bitmap)

    (init parent label
          x y
          style font)

    (define bitmap? (label . is-a? . bitmap%))

    (define/public (get-class) "PLTSTATIC")
    
    (super-new [callback void]
               [parent parent]
               [hwnd 
                (CreateWindowExW/control 0
                                         (get-class)
                                         (if (string? label)
                                             label
                                             "<image>")
                                         (bitwise-ior (if (string? label)
							  SS_LEFTNOWORDWRAP
							  SS_LEFT)
						      WS_CHILD WS_CLIPSIBLINGS
                                                      (if bitmap?
                                                          SS_BITMAP
                                                          (if (symbol? label)
                                                              SS_ICON
                                                              0)))
                                         0 0 0 0
                                         (send parent get-content-hwnd)
                                         #f
                                         hInstance
                                         #f)]
               [style style])

    (when bitmap?
      (let ([hbitmap (bitmap->hbitmap label)])
        (remember-label-bitmap hbitmap)
        (SendMessageW (get-hwnd) STM_SETIMAGE IMAGE_BITMAP 
                      (cast hbitmap _HBITMAP _LPARAM))))

    (when (symbol? label)
      (SendMessageW (get-hwnd) STM_SETIMAGE IMAGE_ICON
                    (cast (case label
			    [(caution) (force warning-icon)]
			    [(stop) (force error-icon)]
			    [else app-icon])
                          _HICON _LPARAM)))
    
    (set-control-font font)

    (if (symbol? label)
        (set-size #f #f 32 32)
        (auto-size font (strip-& label) 0 0 0 0))

    (define/public (set-preferred-size) #f)

    (define/override (get-setimage-message)
      STM_SETIMAGE)))
