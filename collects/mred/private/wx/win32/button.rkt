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

(provide base-button%
         button%)

(define base-button% 
  (class item%
    (inherit set-control-font auto-size get-hwnd)

    (init parent cb label x y w h style font)

    (define callback cb)

    (define bitmap?
      (and (label . is-a? . bitmap%)
           (send label ok?)))

    (define/public (get-class) "BUTTON")
    (define/public (get-flags) BS_PUSHBUTTON)
    
    (super-new [parent parent]
               [hwnd 
                (CreateWindowExW 0
                                 (get-class)
                                 (if (string? label)
                                     label
                                     "<image>")
                                 (bitwise-ior (get-flags) WS_CHILD WS_CLIPSIBLINGS
                                              (if bitmap?
                                                  BS_BITMAP
                                                  0))
                                 0 0 0 0
                                 (send parent get-client-hwnd)
                                 #f
                                 hInstance
                                 #f)]
               [style style])

    (when bitmap?
      (SendMessageW (get-hwnd) BM_SETIMAGE IMAGE_BITMAP 
                    (cast (bitmap->hbitmap label) _HBITMAP _LPARAM)))

    (set-control-font font)

    (define/public (auto-size-button label)
      (cond
       [bitmap?
        (auto-size label 0 0 4 4)]
       [else
        (auto-size label 40 12 12 0)]))
    (auto-size-button label)

    (define/override (is-command? cmd)
      (= cmd BN_CLICKED))

    (define/public (do-command control-hwnd)
      (queue-window-event this (lambda ()
                                 (callback this
                                           (new control-event%
                                                [event-type 'button]
                                                [time-stamp (current-milliseconds)])))))

    (def/public-unimplemented set-border)))

(define button% 
  (class base-button%
    (super-new)))


