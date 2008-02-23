(module wxgroupbox mzscheme
  (require mzlib/class
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "wx.ss"
	   "const.ss"
	   "gdi.ss"
	   "helper.ss"
	   "wxwindow.ss"
	   "wxitem.ss"
	   "wxcanvas.ss")

  (provide (protect wx-group-box%))

  (define group-right-inset 4)

  (define canvas-based-group-box%
    (class* wx-canvas%  (wx-group-box<%>)
      (init mred proxy style parent label style-again _font)

      (define font (or _font small-control-font))
      
      (inherit get-dc get-client-size get-mred
	       set-min-width set-min-height
	       set-tab-focus
	       set-background-to-gray
	       is-enabled-to-root?)
      
      (define lbl label)

      (define lbl-w 0)
      (define lbl-h 0)

      (define/private (compute-sizes)
	(let ([dc (get-dc)])
	  (let-values ([(w h d a) (send dc get-text-extent lbl font)])
	    (set! lbl-w w)
	    (set! lbl-h h))))

      (define/override (on-char e) (void))
      (define/override (on-event e) (void))
      
      (define/override on-paint
	(entry-point
	 (lambda ()
	   (let ([dc (get-dc)])
	     (send dc set-background bg-color)
	     (send dc set-font font)
	     (send dc clear)
	     (send dc set-text-foreground
		   (if (is-enabled-to-root?)
		       black-color
		       disabled-color))
	     (send dc draw-text lbl group-right-inset 0)
	     (send dc set-pen light-pen)
	     (let-values ([(w h) (my-get-client-size)]
			  [(tw th ta td) (send dc get-text-extent lbl)])
	       (send dc draw-line 
		     1 (/ lbl-h 2)
		     (- group-right-inset 2) (/ lbl-h 2))
	       (send dc draw-line
		     1 (/ lbl-h 2)
		     1 (- h 2))
	       (send dc draw-line
		     1 (- h 2)
		     (- w 2) (- h 2))
	       (send dc draw-line
		     (- w 2) (- h 2)
		     (- w 2) (/ lbl-h 2))
	       (send dc draw-line
		     (- w 2) (/ lbl-h 2)
		     (min (- w 2)
			  (+ group-right-inset 4 tw))
		     (/ lbl-h 2)))))))

      (define/private (my-get-client-size)
	(get-two-int-values (lambda (a b) (get-client-size a b))))

      (define/override (handles-key-code code alpha? meta?) 
	#f)

      (define/public (set-label l)
	(set! lbl l)
	(on-paint))

      (super-instantiate (mred proxy parent -1 -1 -1 -1 '(transparent) #f))

      (set-background-to-gray)

      (compute-sizes)
      (set-min-width (inexact->exact (ceiling (+ lbl-w group-right-inset 4))))
      (set-min-height (inexact->exact (ceiling (+ lbl-h 6))))
      (set-tab-focus #f)))

  (define wx-group-box%
    (if (eq? 'unix (system-type))
	canvas-based-group-box%
	(class* (make-window-glue%
		 (make-control% wx:group-box% 0 0 #t #t)) (wx-group-box<%>)
	  (define/override (gets-focus?) #f)
	  (super-instantiate ())))))
