#lang racket/base
(require racket/class
         racket/draw
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

(define message%
  (class item%
    (inherit auto-size set-control-font get-hwnd)

    (init parent label
          x y
          style font)

    (define bitmap?
      (and (label . is-a? . bitmap%)
           (send label ok?)))

    (define/public (get-class) "STATIC")
    
    (super-new [parent parent]
               [hwnd 
                (CreateWindowExW (if (string? label) WS_EX_TRANSPARENT 0)
                                 (get-class)
                                 (if (string? label)
                                     label
                                     "<image>")
                                 (bitwise-ior SS_LEFT WS_CHILD WS_CLIPSIBLINGS
                                              (if bitmap?
                                                  SS_BITMAP
                                                  0))
                                 0 0 0 0
                                 (send parent get-client-hwnd)
                                 #f
                                 hInstance
                                 #f)]
               [style style])

    (when bitmap?
      (SendMessageW (get-hwnd) STM_SETIMAGE IMAGE_BITMAP 
                    (cast (bitmap->hbitmap label) _HBITMAP _LPARAM)))

    (set-control-font font)
        
    (auto-size label 0 0 0 0)))
