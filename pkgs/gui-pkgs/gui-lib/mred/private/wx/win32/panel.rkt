#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
         "window.rkt"
         "wndclass.rkt"
         "utils.rkt"
	 "const.rkt"
         "cursor.rkt")

(provide
 (protect-out panel-mixin
              panel%))

(define (panel-mixin %)
  (class %
    (inherit is-enabled-to-root?
             reset-cursor-in-child
             get-client-hwnd)

    (super-new)

    (define/public (adopt-child child)
      ;; in atomic mode
      (send child set-parent this))

    (define children null)
    (define/override (register-child child on?)
      (let ([on? (and on? #t)]
            [now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (unless on?
            (when (eq? child mouse-in-child)
              (send child send-leaves #f)
              (set! mouse-in-child #f)))
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children)))
          (when on?
            (send child parent-enable (is-enabled-to-root?))))))

    (define/override (internal-enable on?)
      (super internal-enable on?)
      (for ([c (in-list children)])
        (send c parent-enable on?)))

    (define mouse-in-child #f)
    (define/override (generate-mouse-ins in-window mk)
      (unless (eq? in-window this)
        (unless (eq? in-window mouse-in-child)
          (when mouse-in-child
            (send mouse-in-child send-leaves mk))
          (set! mouse-in-child in-window)))
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

    (define/override (show-children)
      (for ([c (in-list children)])
        (send c show-children)))
    (define/override (paint-children)
      (for ([c (in-list children)])
        (send c show-children)))

    (define/override (refresh-all-children)
      (for ([child (in-list children)])
        (send child refresh)))

    (define/override (wants-mouse-capture? control-hwnd)
      (ptr-equal? (get-client-hwnd) control-hwnd))

    (define lbl-pos 'horizontal)
    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))
    
    (define/public (set-item-cursor x y) (void))))

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
                                 (bitwise-ior WS_CHILD WS_CLIPSIBLINGS
                                              (if (memq 'border style)
                                                  WS_BORDER
                                                  0))
                                 0 0 (->screen w) (->screen h)
                                 (send parent get-content-hwnd)
                                 #f
                                 hInstance
                                 #f)]
               [style style])

    ;; For panel in a frame, adjust default cursor to arrow:
    (define arrow-cursor? #f)
    (define/public (set-arrow-cursor) (set! arrow-cursor? #t))
    (define/override (generate-parent-mouse-ins mk)
      (or (super generate-parent-mouse-ins mk)
          (and arrow-cursor?
               (get-arrow-cursor))))))
