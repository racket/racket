#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
          "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "types.rkt")

(provide
 (protect-out gauge%))

(define PBS_VERTICAL #x04)
(define PBM_SETRANGE (+ WM_USER 1))
(define PBM_SETPOS (+ WM_USER 2))
(define PBM_GETRANGE (+ WM_USER 7));wParam = return (TRUE ? low : high). lParam = PPBRANGE or NULL
(define PBM_GETPOS (+ WM_USER 8))

(define gauge%
  (class item%
    (inherit set-size)

    (init parent
          label
          rng
          x y w h
          style
          font)

    (define hwnd
      (CreateWindowExW/control 0
                               "PLTmsctls_progress32"
                               label
                               (bitwise-ior WS_CHILD WS_CLIPSIBLINGS
                                            (if (memq 'vertical style)
                                                PBS_VERTICAL
                                                0))
                               0 0 0 0
                               (send parent get-content-hwnd)
                               #f
                               hInstance
                               #f))

    (super-new [callback void]
               [parent parent]
               [hwnd hwnd]
               [style style])

    (set-range rng)

    (if (memq 'horizontal style)
        (set-size #f #f 100 24)
        (set-size #f #f 24 100))

    (define/public (get-value)
      (SendMessageW hwnd PBM_GETPOS 0 0))
    (define/public (set-value v)
      (void (SendMessageW hwnd PBM_SETPOS v 0)))
    (define/public (get-range)
      (SendMessageW hwnd PBM_GETRANGE 0 0))
    (define/public (set-range v)
      (void (SendMessageW hwnd PBM_SETRANGE 0 (MAKELPARAM 0 v))))))
