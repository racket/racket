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
         "types.rkt")

(provide list-box%)

(define WS_EX_CLIENTEDGE        #x00000200)

(define LBS_NOTIFY            #x0001)
(define LBS_MULTIPLESEL       #x0008)
(define LBS_HASSTRINGS        #x0040)
(define LBS_MULTICOLUMN       #x0200)
(define LBS_WANTKEYBOARDINPUT #x0400)
(define LBS_EXTENDEDSEL       #x0800)
(define LBS_DISABLENOSCROLL   #x1000)

(define LB_ADDSTRING          #x0180)

(define list-box%
  (class item%
    (init parent cb
          label kind x y w h
          choices style
          font label-font)

    (inherit set-size set-control-font)

    (define hwnd
      (CreateWindowExW WS_EX_CLIENTEDGE
                       "LISTBOX"
                       label
                       (bitwise-ior WS_CHILD WS_CLIPSIBLINGS LBS_NOTIFY
                                    WS_VSCROLL
                                    (if (memq 'hscroll style) WS_HSCROLL 0)
                                    (cond
                                     ;; Win32 sense of "multiple" and "extended" is backwards
                                     [(memq 'extended style) LBS_MULTIPLESEL]
                                     [(memq 'multiple style) LBS_EXTENDEDSEL]
                                     [else 0]))
                       0 0 0 0
                       (send parent get-client-hwnd)
                       #f
                       hInstance
                       #f))

    (for ([s (in-list choices)])
      (SendMessageW/str hwnd LB_ADDSTRING 0 s))

    (super-new [parent parent]
               [hwnd hwnd]
               [style style])

    (set-control-font font)
    (set-size -11111 -11111 40 40)

    (def/public-unimplemented get-label-font)
    (def/public-unimplemented set-string)
    (def/public-unimplemented set-first-visible-item)
    (def/public-unimplemented set)
    (def/public-unimplemented get-selections)
    (def/public-unimplemented get-first-item)
    (def/public-unimplemented number-of-visible-items)
    (def/public-unimplemented number)
    (def/public-unimplemented get-selection)
    (def/public-unimplemented set-data)
    (def/public-unimplemented get-data)
    (def/public-unimplemented selected?)
    (def/public-unimplemented set-selection)
    (def/public-unimplemented select)
    (def/public-unimplemented delete)
    (def/public-unimplemented clear)
    (def/public-unimplemented append)))
