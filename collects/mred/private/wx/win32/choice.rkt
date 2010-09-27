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

(provide choice%)

(define CBS_DROPDOWNLIST #x0003)
(define CB_INSERTSTRING #x014A)
(define CB_SETCURSEL #x014E)
(define CB_GETCURSEL #x0147)
(define CBN_SELENDOK 9)

(define choice% 
  (class item%
    (init parent cb label
          x y w h
          choices style font)
    (inherit auto-size set-control-font
             set-size
             subclass-control)

    (define callback cb)
    
    (define hwnd
      (CreateWindowExW 0
                       "PLTCOMBOBOX"
                       label
                       (bitwise-ior WS_CHILD CBS_DROPDOWNLIST 
                                    WS_HSCROLL WS_VSCROLL
                                    WS_BORDER WS_CLIPSIBLINGS)
                       0 0 0 0
                       (send parent get-client-hwnd)
                       #f
                       hInstance
                       #f))

    (define num-choices (length choices))

    (for ([s (in-list choices)]
          [i (in-naturals)])
      (SendMessageW/str hwnd CB_INSERTSTRING i s))

    (SendMessageW hwnd CB_SETCURSEL 0 0)

    (super-new [parent parent]
               [hwnd hwnd]
               [style style])

    (set-control-font font)
    ;; setting the choice height somehow sets the 
    ;; popup-menu size, not the control that you see
    (auto-size choices 0 0 40 0
               (lambda (w h)
                 (set-size -11111 -11111 w (* h 8))))


    (subclass-control hwnd)

    (define/override (is-command? cmd)
      (= cmd CBN_SELENDOK))

    (define/public (do-command control-hwnd)
      (queue-window-event this (lambda ()
                                 (callback this
                                           (new control-event%
                                                [event-type 'choice]
                                                [time-stamp (current-milliseconds)])))))

    
    (define/public (set-selection i)
      (SendMessageW hwnd CB_SETCURSEL i 0))

    (define/public (get-selection)
      (SendMessageW hwnd CB_GETCURSEL 0 0))

    (define/public (number) num-choices)

    (def/public-unimplemented clear)
    (def/public-unimplemented append)))

