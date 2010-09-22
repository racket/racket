#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt")

(provide button%)

(defclass button% item%
  (inherit auto-size)

  (init parent cb label x y w h style font)

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

  (def/public-unimplemented set-border))
