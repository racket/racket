#lang racket/base
(require racket/class
         ffi/unsafe
         "../../syntax.rkt"
         "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "panel.rkt"
	 "wndclass.rkt"
         "types.rkt")

(provide
 (protect-out group-panel%))


(define group-panel% 
  (class (item-mixin (panel-mixin window%))
    (init parent
          x y w h
          style
          label)

    (inherit auto-size set-control-font)

    (define hwnd
      (CreateWindowExW/control 0
                               "PLTBUTTON"
                               (or label "")
                               (bitwise-ior BS_GROUPBOX WS_CHILD WS_CLIPSIBLINGS)
                               0 0 0 0
                               (send parent get-client-hwnd)
                               #f
                               hInstance
                               #f))

    (define client-hwnd
      (CreateWindowExW 0
                       "PLTTabPanel"
                       #f
                       (bitwise-ior WS_CHILD WS_VISIBLE)
                       0 0 (->screen w) (->screen h)
                       hwnd
                       #f
                       hInstance
                       #f))

    (super-new [callback void]
               [parent parent]
               [hwnd hwnd]
               [extra-hwnds (list client-hwnd)]
               [style style])

    (define/override (get-client-hwnd)
      client-hwnd)

    (define label-h 0)
    
    (set-control-font #f)
    (auto-size #f label 0 0 0 0
               (lambda (w h)
                 (set! label-h h)
                 (set-size #f #f (+ w 10) (+ h 10))))

    (define/public (set-label lbl)
      (SetWindowTextW hwnd lbl))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (unless (or (= w -1) (= h -1))
        (MoveWindow client-hwnd (->screen 3) (->screen (+ label-h 3))
		    (->screen (- w 6)) (->screen (- h label-h 6)) #t)))))
