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
         "hbitmap.rkt"
         "types.rkt")

(provide 
 (protect-out base-button%
              button%))

(define BM_SETSTYLE #x00F4)

(define base-button% 
  (class item%
    (inherit set-control-font auto-size get-hwnd
             remember-label-bitmap)

    (init parent cb label x y w h style font)

    (define callback cb)

    (define bitmap? (or (label . is-a? . bitmap%)
			(pair? label)))
    (define orientation (and (pair? label)
			     (caddr label)))

    (define/public (get-class) "PLTBUTTON")
    (define/public (get-flags) BS_PUSHBUTTON)
    
    (super-new [callback cb]
               [parent parent]
               [hwnd 
                (CreateWindowExW/control 0
                                         (get-class)
                                         (cond
					  [(string? label) label]
					  [(pair? label) (cadr label)]
					  [else "<image>"])
                                         (bitwise-ior (get-flags) WS_CHILD WS_CLIPSIBLINGS
                                                      (if bitmap?
							  (case orientation
							    [(#f) BS_BITMAP]
							    [(left) BS_RIGHT]
							    [(right) BS_LEFT]
							    [(top) BS_BOTTOM]
							    [(bottom) BS_TOP])
                                                          0))
                                         0 0 0 0
                                         (send parent get-client-hwnd)
                                         #f
                                         hInstance
                                         #f)]
               [style style])

    (when bitmap?
      (let ([hbitmap (bitmap->hbitmap (if (pair? label) (car label) label)
				      #:bg (get-button-background))])
        (remember-label-bitmap hbitmap)
        (SendMessageW (get-hwnd) BM_SETIMAGE IMAGE_BITMAP 
                      (cast hbitmap _HBITMAP _LPARAM))))

    (set-control-font font)

    (define/public (get-button-background)
      #xFFFFFF)

    (define/public (auto-size-button font label)
      (cond
       [orientation
	(let ([h? (memq orientation '(left right))])
	  (auto-size font (list (car label) (cadr label))
		     0 0 4 4
		     #:combine-width (if h? + max)
		     #:combine-height (if h? max +)))]
       [bitmap?
        (auto-size font label 0 0 4 4)]
       [else
        (auto-size font label 60 20 12 0 #:scale-w 1.1 #:scale-h 1.1)]))
    (auto-size-button font label)

    (define/override (is-command? cmd)
      (= cmd BN_CLICKED))

    (define/public (do-command cmd control-hwnd)
      (queue-window-event this (lambda ()
                                 (callback this
                                           (new control-event%
                                                [event-type 'button]
                                                [time-stamp (current-milliseconds)])))))

    (define/public (set-border on?)
      (SendMessageW (get-hwnd) BM_SETSTYLE
                    (if on? BS_DEFPUSHBUTTON BS_PUSHBUTTON)
                    1))))

(define button% 
  (class base-button%
    (super-new)))


