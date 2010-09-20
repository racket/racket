#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt"
         "wndclass.rkt"
	 "const.rkt")

(provide panel%)

(defclass panel% window%
  (init parent
        x y w h
        style
        label)

  (super-new [parent parent]
	     [win32 
	      (CreateWindowExW 0
			       "PLTPanel"
			       #f
			       (bitwise-ior WS_CHILD)
			       0 0 w h
			       (send parent get-win32)
			       #f
			       hInstance
			       #f)]
	     [style style])

  (def/public-unimplemented get-label-position)
  (def/public-unimplemented set-label-position)
  (def/public-unimplemented on-char)
  (def/public-unimplemented on-event)
  (def/public-unimplemented on-paint)
  (define/public (set-item-cursor x y) (void))
  (def/public-unimplemented get-item-cursor))
