#lang racket/base
(require racket/class
         racket/draw
	 racket/draw/private/xp
         ffi/unsafe
          "../../syntax.rkt"
          "../../lock.rkt"
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
             remember-label-bitmap set-size)

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
							  (case (and (not xp?) 
								     orientation)
							    [(#f) BS_BITMAP]
							    [(left) BS_LEFT]
							    [(right) BS_RIGHT]
							    [(top) BS_TOP]
							    [(bottom) BS_BOTTOM])
                                                          0))
                                         0 0 0 0
                                         (send parent get-content-hwnd)
                                         #f
                                         hInstance
                                         #f)]
               [style style])

    (when bitmap?
      (let ([hbitmap (bitmap->hbitmap (if (pair? label)
					  (if xp?
					      (collapse-to-bitmap label font)
					      (car label))
					  label)
				      #:bg (get-button-background))])
        (remember-label-bitmap hbitmap)
        (SendMessageW (get-hwnd) BM_SETIMAGE IMAGE_BITMAP 
                      (cast hbitmap _HBITMAP _LPARAM))))

    (define/private (collapse-to-bitmap label font)
      ;; XP doesn't handle a combination of string
      ;; and bitmap labels
      (let-values ([(w h) (auto-size-button font label
					    #:resize (lambda (w h)
						       (values w h)))])
	 (let* ([bm (make-object bitmap% w h #f #f)]
		[dc (make-object bitmap-dc% bm)]
		[h? (memq (caddr label) '(left right))])
	   (send dc draw-bitmap (car label)
		 (if h?
		     (if (eq? (caddr label) 'left)
			 3
			 (- w (send (car label) get-width) 3))
		     (quotient (- w (send (car label) get-width)) 2))
		 (if h?
		     (quotient (- h (send (car label) get-height)) 2)
		     (if (eq? (caddr label) 'top)
			 3
			 (- h (send (car label) get-height) 3))))
	   (send dc set-font (or font (get-default-control-font)))
	   (let-values ([(tw th ta td) (send dc get-text-extent (cadr label))])
	     (send dc draw-text (cadr label)
		   (if h?
		       (if (eq? (caddr label) 'left)
			   (- w tw 3)
			   3)
		       (quotient (- w tw) 2))
		   (if h?
		       (quotient (- h th) 2)
		       (if (eq? (caddr label) 'top)
			   (- h th 3)
			   3))))
	   (send dc set-bitmap #f)
	   bm)))

    (set-control-font font)

    (define/public (get-button-background)
      #xFFFFFF)

    (define/public (auto-size-button 
		    font 
		    label
		    #:resize [resize (lambda (w h) (set-size -11111 -11111 w h))])
      (cond
       [orientation
	(let ([h? (memq orientation '(left right))])
	  (auto-size font (list (car label) (cadr label))
		     0 0 12 8
		     resize
		     #:combine-width (if h? + max)
		     #:combine-height (if h? max +)))]
       [bitmap?
        (auto-size font label 0 0 4 4)]
       [else
        (auto-size font label 60 20 12 0 #:scale-w 1.1 #:scale-h 1.1)]))
    (auto-size-button font label)

    ;; XP doesn't show both bitmap and string labels,
    ;; so we synthesize a bitmap label when we have both
    (define xp-label-bitmap (and xp? orientation (car label)))
    (define xp-label-string (and xp? orientation (string->immutable-string (cadr label))))
    (define xp-label-font (and xp? orientation font))

    (define/override (set-label s)
      (if (and orientation xp?)
	  (atomically
	   (begin
	     (if (string? s) 
		 (set! xp-label-string s)
		 (set! xp-label-bitmap s))
	     (super
	      set-label
	      (collapse-to-bitmap (list xp-label-bitmap
					xp-label-string
					orientation)
				  xp-label-font))))
	  (super set-label s)))

    (define/override (is-command? cmd)
      (= cmd BN_CLICKED))

    (define/override (do-command cmd control-hwnd)
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
