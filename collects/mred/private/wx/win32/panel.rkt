#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt"
         "wndclass.rkt"
	 "const.rkt")

(provide panel-mixin
         panel%)

(define (panel-mixin %)
  (class %
    (super-new)
    
    (define lbl-pos 'horizontal)
    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))
    
    (def/public-unimplemented on-paint)
    (define/public (set-item-cursor x y) (void))
    (def/public-unimplemented get-item-cursor)))

(define panel% 
  (class (panel-mixin window%)
    (init parent
          x y w h
          style
          label)

    (super-new [parent parent]
               [hwnd 
                (CreateWindowExW 0
                                 (if (send parent is-frame?)
                                     "PLTPanel"
                                     "PLTTabPanel")
                                 #f
                                 (bitwise-ior WS_CHILD)
                                 0 0 w h
                                 (send parent get-client-hwnd)
                                 #f
                                 hInstance
                                 #f)]
               [style style])))
