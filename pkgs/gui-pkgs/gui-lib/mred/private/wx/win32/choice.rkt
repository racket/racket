#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
          "../../syntax.rkt"
          "../../lock.rkt"
          "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "types.rkt")

(provide 
 (protect-out choice%))

(define choice% 
  (class item%
    (init parent cb label
          x y w h
          choices style font)
    (inherit auto-size set-control-font
             set-size)

    (define callback cb)
    
    (define hwnd
      (CreateWindowExW/control 0
                               "PLTCOMBOBOX"
                               label
                               (bitwise-ior WS_CHILD CBS_DROPDOWNLIST 
                                            WS_HSCROLL WS_VSCROLL
                                            WS_BORDER WS_CLIPSIBLINGS)
                               0 0 0 0
                               (send parent get-content-hwnd)
                               #f
                               hInstance
                               #f))

    (define num-choices (length choices))

    (for ([s (in-list choices)]
          [i (in-naturals)])
      (SendMessageW/str hwnd CB_INSERTSTRING i s))

    (SendMessageW hwnd CB_SETCURSEL 0 0)

    (super-new [callback cb]
               [parent parent]
               [hwnd hwnd]
               [style style])

    (set-control-font font)
    ;; setting the choice height somehow sets the 
    ;; popup-menu size, not the control that you see
    (auto-size font
               (if (null? choices) (list "Choice") choices)
               0 0 40 0
               (lambda (w h)
                 (set-size #f #f w (* h 8))))


    (define choice-dropped? #f)

    (define/override (ctlproc w msg wParam lParam default)
      (cond
       [(and choice-dropped?
             (or (= msg WM_KEYDOWN)
                 (= msg WM_KEYUP)
                 (= msg WM_SYSCHAR)
                 (= msg WM_CHAR)))
        (default w msg wParam lParam)]
       [else (super ctlproc w msg wParam lParam default)]))

    (define/override (is-command? cmd)
      (when (= cmd CBN_DROPDOWN)
        (set! choice-dropped? #t))
      (when (= cmd CBN_CLOSEUP)
        (queue-window-event this (lambda ()
                                   (set! choice-dropped? #f))))
      (= cmd CBN_SELENDOK))

    (define/override (do-command cmd control-hwnd)
      (queue-window-event this (lambda ()
                                 (callback this
                                           (new control-event%
                                                [event-type 'choice]
                                                [time-stamp (current-milliseconds)])))))

    
    (define/public (set-selection i)
      (void (SendMessageW hwnd CB_SETCURSEL i 0)))

    (define/public (get-selection)
      (SendMessageW hwnd CB_GETCURSEL 0 0))

    (define/public (number) num-choices)

    (define/public (clear)
      (atomically
       (SendMessageW hwnd CB_RESETCONTENT 0 0)
       (set! num-choices 0)))

    (public [append* append])
    (define (append* str)
      (atomically
       (SendMessageW/str hwnd CB_ADDSTRING 0 str)
       (set! num-choices (add1 num-choices))
       (when (= 1 num-choices) (set-selection 0))))

    (define/public (delete i)
      (set! num-choices (sub1 num-choices))
      (void (SendMessageW hwnd CB_DELETESTRING i 0)))))
