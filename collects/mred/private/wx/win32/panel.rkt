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
    (inherit is-enabled-to-root?
             reset-cursor-in-child)

    (super-new)

    (define children null)
    (define/override (register-child child on?)
      (let ([now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (unless on?
            (when (eq? child mouse-in-child)
              (set! mouse-in-child #f)))
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children)))
          (send child parent-enable (is-enabled-to-root?)))))

    (define/override (internal-enable on?)
      (super internal-enable on?)
      (for ([c (in-list children)])
        (send c parent-enable on?)))

    (define mouse-in-child #f)
    (define/override (generate-mouse-ins in-window mk)
      (unless (eq? in-window mouse-in-child)
        (when mouse-in-child
          (send mouse-in-child send-leaves mk))
        (set! mouse-in-child in-window))
      (super generate-mouse-ins in-window mk))

    (define/override (reset-cursor default)
      (if mouse-in-child
          (reset-cursor-in-child mouse-in-child default)
          (super reset-cursor default)))

    (define/override (send-leaves mk)
      (when mouse-in-child
        (let ([w mouse-in-child])
          (set! mouse-in-child #f)
          (send w send-leaves mk)))
      (super send-leaves mk))

    (define/override (send-child-leaves mk)
      (if mouse-in-child
          (let ([w mouse-in-child])
            (set! mouse-in-child #f)
            (send w send-leaves mk)
            #t)
          #f))

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
