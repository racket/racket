#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
          "../common/event.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt")

(provide button%)

(defclass button% item%
  (inherit auto-size)

  (init parent cb label x y w h style font)

  (define callback cb)

  (super-new [parent parent]
	     [hwnd 
	      (CreateWindowExW 0
			       "BUTTON"
			       label
			       (bitwise-ior BS_PUSHBUTTON WS_CHILD WS_CLIPSIBLINGS)
			       0 0 0 0
			       (send parent get-hwnd)
			       #f
			       hInstance
			       #f)]
	     [style style])

  (auto-size label 40 12 12 0)

  (define/public (do-command)
    (queue-window-event this (lambda ()
                               (callback this
                                         (new control-event%
                                              [event-type 'button]
                                              [time-stamp (current-milliseconds)])))))

  (def/public-unimplemented set-border))
