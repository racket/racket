(module wxtabgroup mzscheme
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

  (provide (protect wx-tab-group%
		    canvas-based-tab-group%))

  (define mac-tab? (eq? 'macosx (system-type)))

  (define tab-v-space 2)
  (define raise-h (if mac-tab? 0 2))

  (define canvas-based-tab-group%
    (class* wx-canvas% (wx-tab-group<%>)
      (init mred proxy style parent call-back label tab-labels style-again _font)
      
      (define callback call-back)

      (define tabs (map wx:label->plain-label tab-labels))
      (define tab-widths #f)
      (define tab-height #f)

      (define current-focus-tab 0)
      
      (define font (or _font normal-control-font))
      
      (inherit get-dc get-client-size get-mred
	       set-min-width set-min-height
	       set-tab-focus set-focus has-focus?
	       set-background-to-gray refresh
	       get-top-level is-enabled-to-root?)
      
      (define selected 0)
      (define tracking-pos #f)
      (define tracking-hit? #f)
      
      (define regions #f)
      (define redo-regions? #f)

      (define border? (memq 'border style))

      (define/private (compute-sizes)
	(let ([dc (get-dc)])
	  (let ([w+hs (map (lambda (lbl)
			     (let-values ([(w h d a) (send dc get-text-extent lbl font)])
			       (cons w h)))
			   tabs)])
	    (set! tab-widths (map car w+hs))
	    (if mac-tab?
		(set! tab-height (+ 27 (send font get-point-size) -13))
		(let-values ([(sw sh sd sa) (send dc get-text-extent " " font)])
		  (let ([th (ceiling (+ (* 2 tab-v-space) (apply max 0 sh (map cdr w+hs))))])
		    (set! tab-height (if (even? th) th (add1 th)))))))))
      
      (define/private (get-total-width)
	(compute-sizes)
	(apply + 
	       (if mac-tab? 0 tab-height)
	       (* (length tabs) (+ raise-h raise-h tab-height)) 
	       tab-widths))
      
      (define/private (get-init-x)
	(if border?
	    (let-values ([(w h) (my-get-client-size)]
			 [(tw) (get-total-width)])
	      (/ (- w tw) 2))
	    (if mac-tab?
		2
		0)))

      (define/override (on-char e) (void))

      (define/override on-event
	(entry-point
	 (lambda (e)
	   (cond
	    [(and (send e button-down?) tab-widths)
	     (set! tracking-pos (find-click (send e get-x) (send e get-y)))
	     (when tracking-pos
	       (set! current-focus-tab tracking-pos)
	       (set! tracking-hit? #t)
	       (update-tracking))]
	    [(and (send e dragging?) tracking-pos)
	     (let ([hit? (equal? tracking-pos (find-click (send e get-x) (send e get-y)))])
	       (unless (eq? tracking-hit? hit?)
		 (set! tracking-hit? hit?)
		 (update-tracking)))]
	    [(and (send e button-up?) tracking-pos
		  (equal? tracking-pos (find-click (send e get-x) (send e get-y)))
		  (not (= tracking-pos selected)))
	     ;; Button released for final selection
	     (let ([new tracking-pos])
	       (set! tracking-pos #f)
	       (set! tracking-hit? #f)
	       (set-selection new)
	       (as-exit
		(lambda ()
		  (callback this (make-object wx:control-event% 'tab-panel)))))]
	    ;; otherwise, turn off tracking...
	    [else
	     (when tracking-hit?
	       (set! tracking-hit? #f)
	       (update-tracking))
	     (set! tracking-pos #f)]))))
      
      (define/private (update-tracking)
	(if mac-tab?
	    (refresh)
	    (let ([dc (get-dc)])
	      (send dc set-clipping-region (list-ref regions tracking-pos))
	      (on-paint)
	      (send dc set-clipping-region #f))))
	
      (define tmp-rgn #f)

      (define/public (button-focus n)
	(if (< n 0)
	    current-focus-tab
	    (begin
	      (set! current-focus-tab n)
	      (refresh)
	      (set-focus)
	      current-focus-tab)))

      (define/override on-set-focus
	(lambda ()
	  (refresh)
	  (super on-set-focus)))
      (define/override on-kill-focus
	(lambda ()
	  (refresh)
	  (super on-kill-focus)))
      
      (define/private (find-click x y)
	(ready-regions)
	(unless tmp-rgn
	  (set! tmp-rgn (make-object wx:region% (get-dc))))
	(let loop ([rl regions][pos 0])
	  (if (null? rl)
	      #f
	      (begin
		(send tmp-rgn set-rectangle x y 1 1)
		(send tmp-rgn intersect (car rl))
		(if (send tmp-rgn is-empty?)
		    (loop (cdr rl) (add1 pos))
		    pos)))))
      
      (define/private (setup-regions)
	(let ([dc (get-dc)])
	  (set! regions
		(map (lambda (tpl r)
		       (let ([points (map (lambda (p) (make-object wx:point% (car p) (+ 2 raise-h (cadr p))))
					  tpl)])
			 (send r set-polygon points))
		       r)
		     (draw-once #f 0 #f #f 0 #f)
		     (if regions
			 regions
			 (map (lambda (x)
				(make-object wx:region% dc))
			      tabs))))
	  (set! redo-regions? #f)))

      (define/private (ready-regions)
	(compute-sizes)
	(unless (and regions (not redo-regions?)) 
	  (setup-regions)))

      (define/override (gets-focus?) #t)    
      (define/override (tabbing-position x y w h)
	(list this (+ x (get-init-x)) y (get-total-width) tab-height))
      (define/public (number) (length tabs))
      
      ;; Returns a list of point lists, which define polygons for hit-testing
      ;;  and updating
      (define/private (draw-once dc w light? dark? inset active?)
	(let ([init-x (get-init-x)])
	  (let loop ([x init-x][l tabs][wl tab-widths][pos 0])
	    (if (null? l)
		null
		(let ([next-x (+ x tab-height (car wl))]
		      [-sel-d (if (= pos selected) (- raise-h) 0)])
		  (cons
		   (if mac-tab?
		       ;; ----- Mac drawing -----
		       (let ([w (+ tab-height (car wl))]
			     [h tab-height])
			 (when dc
			   (when (eq? dark? (= pos selected))
			     (wx:draw-tab 
			      dc
			      (car l) x 3 w (- tab-height 3)
			      (+ (if (and (has-focus?)
					  (= pos current-focus-tab))
				     ;; Adding 100 means "draw focus ring"
				     100
				     ;; No focus
				     0)
				 ;; Pick the style: active and front, etc.
				 (if (and light?
					  (eq? pos tracking-pos))
				     1
				     (if active?
					 (if dark? 3 0)
					 (if dark? 4 2)))))))
			 (list (list x 3) (list (+ x w) 3)
			       (list (+ x w) (- tab-height 6)) (list x (- tab-height 6))))
		       ;; ----- X-style drawing -----
		       (append
			;; start point
			(list (list (+ x tab-height -sel-d inset) (+ 2 tab-height (- inset))))
			;; left line
			(begin
			  (when (= pos selected)
			    (when light?
			      (send dc set-pen border-pen)
			      (send dc draw-line 0 tab-height (sub1 x) tab-height)
			      (send dc set-pen light-pen)
			      (send dc draw-line 0 (add1 tab-height) (sub1 x) (add1 tab-height))))
			  (let ([short (if (or (= pos 0) (= pos selected))
					   0
					   (+ (/ tab-height 2) 
					      (if (= selected (sub1 pos))
						  raise-h
						  0)))])
			    (when light?
			      (send dc set-pen border-pen)
			      (send dc draw-line (+ x short -sel-d) (- tab-height short) (+ x tab-height) -sel-d)
			      (send dc set-pen light-pen)
			      (send dc draw-line (+ x short -sel-d 1) (- tab-height short) (+ x tab-height 1) -sel-d))
			    (list (list (+ x short -sel-d -2 inset) (- tab-height short -2 inset))
				  (list (+ x tab-height inset) (+ -sel-d inset)))))
			;; top line
			(begin
			  (when light?
			    (send dc set-pen border-pen)
			    (send dc draw-line (+ x tab-height) -sel-d next-x -sel-d)
			    (send dc set-pen light-pen)
			    (send dc draw-line (+ x tab-height) (+ 1 -sel-d) next-x (+ 1 -sel-d)))
			  (list (list (+ 1 next-x (- inset)) (+ inset -sel-d))))
			;; right line
			(let* ([short (if (= (add1 pos) selected)
					  (+ (/ tab-height 2) (sub1 raise-h))
					  0)]
			       [short-d (if (zero? short) 0 -1)])
			  (when dark?
			    (send dc set-pen border-pen)
			    (send dc draw-line next-x (+ -sel-d 1) 
				  (- (+ next-x tab-height) short 2 -sel-d short-d) (- tab-height short 1 short-d))
			    (send dc set-pen dark-pen)
			    (send dc draw-line (+ 1 next-x) (+ -sel-d 1) (- (+ next-x tab-height) short 1 -sel-d) (- tab-height short 1)))
			  (list (list (- (+ next-x tab-height) -sel-d short (- short-d) -2 inset) (- tab-height short -2 inset))))
			;; end point
			(begin
			  (when light?
			    (when (= pos selected)
			      (send dc set-pen border-pen)
			      (send dc draw-line (+ next-x tab-height) tab-height w tab-height)
			      (send dc set-pen light-pen)
			      (send dc draw-line (+ next-x tab-height) (add1 tab-height) w (add1 tab-height)))
			    (let ([x (+ x tab-height)]
				  [y (- tab-v-space (if (= pos selected) raise-h 0))])
			      (send dc set-text-foreground
				    (if (is-enabled-to-root?)
					black-color
					disabled-color))
			      (send dc draw-text (car l) x y)
			      (when (and (has-focus?)
					 (= pos current-focus-tab))
				(let ([p (send dc get-pen)])
				  (send dc set-pen "black" 1 'hilite)
				  (let ([x (- x 1)]
					[y (+ y 2)]
					[w (+ (car wl) 2)]
					[h (- tab-height (* 2 tab-v-space) 2)])
				    (send dc draw-line (+ x 0) (+ y -1) (+ x w -1) (+ y -1))
				    (send dc draw-line (+ x 0) (+ y h) (+ x w -1) (+ y h))
				    (send dc draw-line (+ x -1) (+ y 0) (+ x -1) (+ y h -1))
				    (send dc draw-line (+ x w) (+ y 0) (+ x w) (+ y h -1)))
				  (send dc set-pen p)))))
			  (list (list (+ next-x inset (if (= selected (add1 pos)) -2 0)) (+ 2 tab-height (- inset)))))))
		   (loop next-x (cdr l) (cdr wl) (add1 pos))))))))

      
      (define/override on-paint
	(entry-point
	 (lambda ()
	   (compute-sizes)
	   (let ([dc (get-dc)]
		 [active? (and (is-enabled-to-root?)
			       (send (get-top-level) is-act-on?))])
	     (send dc set-background bg-color)
	     (send dc set-font font)
	     (unless mac-tab?
	       (send dc clear)
	       (send dc set-origin 0 (+ 2 raise-h))
	       (when (and tracking-pos tracking-hit?)
		 (let ([b (send dc get-brush)])
		   (send dc set-brush dark-brush)
		   (send dc set-pen trans-pen)
		   (send dc draw-polygon (map (lambda (x) (make-object wx:point% (car x) (cadr x)))
					      (list-ref (draw-once #f 0 #f #f 1 #f) tracking-pos)))
		   (send dc set-brush b))))
	     (let-values ([(w h) (my-get-client-size)])
	       (unless mac-tab?
		 (send dc set-pen light-pen))
	       (draw-once dc w #t #f 0 active?)
	       (when mac-tab?
		 (wx:draw-tab-base dc 0 (- tab-height 3) w 6 (if active? 1 0)))
	       (when border?
		 (when (> h tab-height)
		   (send dc draw-line 1 (add1 tab-height) 1 h)
		   (send dc set-pen border-pen)
		   (send dc draw-line 0 tab-height 0 h)))
	       (unless mac-tab?
		 (send dc set-pen dark-pen))
	       (draw-once dc w #f #t 0 active?)
	       (when border?
		 (when (> h tab-height)
		   (send dc draw-line (- w 2) (+ 1 tab-height) (- w 2) (- h raise-h))
		   (send dc draw-line 1 (- h 4 raise-h) w (- h 4 raise-h))
		   (send dc set-pen border-pen)
		   (send dc draw-line (- w 1) tab-height (- w 1) (- h raise-h))
		   (send dc draw-line 0 (- h 3 raise-h) w (- h 3 raise-h)))))
	     (send dc set-origin 0 0)))))

      (define/override (on-size w h)
	(set! redo-regions? #t)
	(super on-size w h))

      (define/private (my-get-client-size)
	(get-two-int-values (lambda (a b) (get-client-size a b))))
      
      (define/public (get-selection)
	selected)

      (define/public (set-selection i)
	(ready-regions)
	(when (< -1 i (length regions))
	  (let* ([dc (get-dc)]
		 [r (make-object wx:region% dc)]
		 [old-rgn (list-ref regions selected)])
	    (set! selected i)
	    (send r union old-rgn)
	    (setup-regions)
	    (if mac-tab?
		(refresh) ;; but we need an immediate refresh!
		(let ([new-rgn (list-ref regions selected)])
		  ;; Union the new and old regions and repaint:
		  (send r union new-rgn)
		  (send dc set-clipping-region r)
		  (on-paint)
		  (send dc set-clipping-region #f))))))

      (define/public (set-label i s)
        (set! tabs (let loop ([tabs tabs][i i])
                     (if (zero? i)
                         (cons (wx:label->plain-label s) (cdr tabs))
                         (cons (car tabs) (loop (cdr tabs) (sub1 i))))))
	(set! tab-widths #f)
	(set! regions #f)
	(refresh))

      (define/public (set tab-labels)
	(set! tabs (map wx:label->plain-label tab-labels))
	(set! tab-widths #f)
	(set! regions #f)
	(set! selected (max 0 (min selected (sub1 (length tabs)))))
	(refresh))

      (define (-append s)
	(set! tabs (append tabs (list (wx:label->plain-label s))))
	(set! tab-widths #f)
	(set! regions #f)
	(refresh))
      (public (-append append))

      (define/public (delete i)
	(set! tabs (let loop ([pos 0][tabs tabs])
		     (if (= i pos)
			 (cdr tabs)
			 (cons (car tabs) (loop (add1 pos) (cdr tabs))))))
	(set! selected (min (if (selected . <= . i)
				selected
				(sub1 selected))
			    (max 0 (sub1 (length tabs)))))
	(set! regions #f)
	(set! tab-widths #f)
	(refresh))
      
      (define/override (handles-key-code code alpha? meta?) 
	#f)

      (super-instantiate (mred proxy parent -1 -1 -1 -1 '(transparent) #f))
      
      (let ([focus-ok? 
	     ;; For Mac OS X, this method indicates that the
	     ;;  canvas should not necessarily get the focus
	     ;;  on a click, and the result indicates whether
	     ;;  it should accept tab focus in general
	     (set-background-to-gray)])

	(compute-sizes)
	(set-min-width (inexact->exact (ceiling (get-total-width))))
	(set-min-height (inexact->exact (ceiling (+ tab-height (if mac-tab? 6 9) raise-h))))
	(when mac-tab?
	  (send (get-top-level) add-activate-update this))
	(set-tab-focus focus-ok?))))

  (define wx-tab-group% 
    (if (eq? 'unix (system-type))
	canvas-based-tab-group%
	(class* (make-window-glue%
		 (make-control% wx:tab-group% 0 0 #t #t)) (wx-tab-group<%>)
	  (inherit min-height)
	  (define/override (tabbing-position x y w h)
	    (list this x y w (min-height)))
	  (define/override (handles-key-code code alpha? meta?) #f)
	  (super-instantiate ())))))
