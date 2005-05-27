(unit/sig loa:grid^
  (import mzlib:function^
	  mred^
	  loa:utils^)

  (define black-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define black-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))

  (define highlight-color "PALE GREEN")
  (define highlight-pen (send the-pen-list find-or-create-pen highlight-color 1 'solid))
  (define highlight-brush (send the-brush-list find-or-create-brush highlight-color 'solid))

  (define line-color "CORNFLOWER BLUE")
  (define line-pen (send the-pen-list find-or-create-pen line-color 1 'solid))
  (define line-brush (send the-brush-list find-or-create-brush line-color 'solid))

  (define grid-pasteboard%
    (class pasteboard% (x-size y-size . args)
      (inherit get-canvas find-first-snip move-to invalidate-bitmap-cache)
      (private
	[calculate-grid
	 (lambda (entries pixels)
	   (build-vector
	    (+ entries 1)
	    (lambda (i) (* i (/ pixels entries)))))])

      (private
	[margin 4]
	[pieces (build-vector x-size (lambda (i) (build-vector y-size (lambda (j) null))))]
	[y-grid (calculate-grid x-size (* x-size 2))]
	[x-grid (calculate-grid y-size (* y-size 2))])

      (public
	[get-margin (lambda () margin)]
	[set-margin (lambda (m) (when (>= m 4) (set! margin m)))])

      (public
	[get-moves
	 (lambda (snip) (list (cons (send snip get-x) (send snip get-y))))])

      (private
	[valid-move? (lambda (snip cx cy)
		       (let ([legal-moves (get-moves snip)])
			 (member
			  (cons cx cy)
			  legal-moves)))])

      (private
	[grid-xy->pixel-xywh
	 (lambda (x y)
	   (let*-values ([(canvas) (get-canvas)]
			 [(bx by) (send canvas get-client-size)])
	     (values (* x (/ bx x-size))
		     (* y (/ by y-size))
		     (/ bx x-size)
		     (/ by y-size))))]
			 

	[pixel-xy->grid-xy
	 (lambda (px py)
	   (let*-values ([(canvas) (get-canvas)]
			 [(bx by) (send canvas get-client-size)]
			 [(gx) (floor (* x-size (/ px bx)))]
			 [(gy) (floor (* y-size (/ py by)))])
	     (values (inexact->exact gx) (inexact->exact gy))))])

      (private
	[cursor-x/y #f]
	[update-cursor-x/y
	 (lambda (new-x/y)
	   (unless (equal? cursor-x/y new-x/y)
	     (set! cursor-x/y new-x/y)
	     (invalidate-bitmap-cache)))])

      (rename [super-on-local-event on-local-event])
      (override
       [on-local-event
	(lambda (evt)
	  (cond
	   [(send evt leaving?)
	    (update-cursor-x/y #f)]
	   [(or (send evt moving?)
		(send evt entering?))
	    (let-values ([(px py) (pixel-xy->grid-xy (send evt get-x) (send evt get-y))])
	      (update-cursor-x/y (cons px py)))]
	   [else (void)])
	  (super-on-local-event evt))])

      (private 
	[ignored-move? #f])
      (public
	[animate-to
	 (lambda (snip x y)
	   (set! ignored-move? #t)
	   (let* ([canvas (get-canvas)])
	     (let-values ([(bx by) (send canvas get-client-size)])
	       (move-to snip 
			(* x (/ bx x-size))
			(* y (/ by y-size))))
	     (set! ignored-move? #f)))])
      (inherit find-next-selected-snip)

      (public
	[moved
	 (lambda (l)
	   (void))])

      (override
	[after-interactive-move
	 (lambda (event)
	   (unless ignored-move?
	     (let ([moved-snips
		    (let-values ([(cx cy) (pixel-xy->grid-xy (send event get-x) (send event get-y))])
		      (let loop ([snip (find-next-selected-snip #f)])
			(if snip
			    (if (valid-move? snip cx cy)
				(begin (send snip set-x cx)
				       (send snip set-y cy)
				       (animate-to snip cx cy)
				       (cons snip (loop (find-next-selected-snip snip))))
				(begin (bell)
				       (animate-to snip (send snip get-x) (send snip get-y))
				       (loop (find-next-selected-snip snip))))
			    null)))])
	       (unless (null? moved-snips)
		 (moved moved-snips)))
	     (invalidate-bitmap-cache)))])

      (rename [super-on-paint on-paint])
      (inherit begin-edit-sequence end-edit-sequence)
      (override
       [on-paint
	(lambda (before dc left top right bottom dx dy draw-caret)
	  (let ([orig-pen (send dc get-pen)]
		[orig-brush (send dc get-brush)])

	    (when cursor-x/y
	      (if before
		  (begin (send dc set-pen highlight-pen)
			 (send dc set-brush highlight-brush))
		  (begin (send dc set-pen line-pen)
			 (send dc set-brush line-brush)))
	      (let ([snip (get-snip-at (car cursor-x/y) (cdr cursor-x/y))])
		(when snip
		  (let ([spots (get-moves snip)])
		    (for-each (lambda (spot)
				(let-values ([(x y w h) (grid-xy->pixel-xywh (car spot) (cdr spot))])
				  (if before
				      (send dc draw-rectangle (+ x dx) (+ y dy) w h)
				      (let-values ([(fx fy fw fh) (grid-xy->pixel-xywh (car cursor-x/y) (cdr cursor-x/y))])
					(send dc draw-line
					      (+ fx (/ fw 2))
					      (+ fy (/ fh 2))
					      (+ x (/ w 2))
					      (+ y (/ h 2)))))))
			      spots)))))

	    (when before
	      (send dc set-pen black-pen)

	      (vector-for-each
	       (get-x-grid)
	       (lambda (x)
		 (send dc draw-line (+ x dx) (+ top dy) (+ x dx) (+ bottom dy))))
	      
	      (vector-for-each
	       (get-y-grid)
	       (lambda (y)
		 (send dc draw-line (+ left dx) (+ y dy) (+ right dx) (+ y dy)))))

	    (super-on-paint before dc left top right bottom dx dy draw-caret)

	    (send dc set-pen orig-pen)
	    (send dc set-brush orig-brush)))])

      (public
	[on-size
	 (lambda (w h)
	   (set! x-grid (calculate-grid x-size w))
	   (set! y-grid (calculate-grid y-size h))
	   (let ([xs (/ w x-size)]
		 [ys (/ h y-size)])
	     (begin-edit-sequence)
	     (let loop ([snip (find-first-snip)])
	       (cond
		 [(not snip) (void)]
		 [else
		  (send snip allow-resize #t)
		  (send snip resize xs ys)
		  (send snip allow-resize #f)
		  (move-to snip 
			   (* xs (send snip get-x))
			   (* ys (send snip get-y)))
		  (loop (send snip next))]))
	     (end-edit-sequence)))])

      (public
	[get-x-grid (lambda () x-grid)]
	[get-y-grid (lambda () y-grid)]
	[get-pieces (lambda () pieces)])

      (inherit insert resize find-snip)
      (public
	[get-snip-at
	 (lambda (x y)
	   (let ([snips (get-all-snips-at x y)])
	     (if (null? snips)
		 #f
		 (car snips))))]
	
	[get-all-snips-at
	 (lambda (x y)
	   (let loop ([snip (find-first-snip)])
	     (cond
	       [snip
		(if (and (= x (send snip get-x))
			 (= y (send snip get-y)))
		    (cons snip (loop (send snip next)))
		    (loop (send snip next)))]
	       [else null])))]

	[insert-at
	 (lambda (snip x y)
	   (send snip set-x x)
	   (send snip set-y y)
	   (let ([canvas (get-canvas)])
	     (if canvas
		 (let-values ([(bx by) (send canvas get-client-size)])
		   (let* ([xw (/ bx x-size)]
			  [yw (/ by y-size)]
			  [cx (* x xw)]
			  [cy (* y yw)])
		     (insert snip cx cy)
		     (resize snip xw yw)))
		 (insert snip 0 0)))
	   (let ([col (vector-ref pieces x)])
	     (vector-set! col y (cons snip (vector-ref col y)))))])
      (sequence
	(apply super-init args))))

  (define grid-canvas%
    (class editor-canvas% args
      (inherit get-editor)
      (rename [super-get-client-size get-client-size])
      (override
	[get-client-size
	 (lambda ()
	   (let-values ([(w h) (super-get-client-size)])
	     (values (max 0 (- w 11))
		     (max 0 (- h 11)))))]
	[on-size
	 (lambda (width height)
	   (let ([media (get-editor)])
	     (when media
	       (let-values ([(w h) (get-client-size)])
		 (send media on-size w h)))))])
      (sequence (apply super-init args))))

  (define grid-snip%
    (class snip% (_x _y)
      (private
	[width 10]
	[height 10])
      (public
	[allow-resize
	 (let ([ans #f])
	   (case-lambda
	    [(x) (set! ans x)]
	    [() ans]))]
	[get-width (lambda () width)]
	[get-height (lambda () height)])
      (inherit get-admin)
      (override
	[resize 
	 (lambda (w h)
	   (and (allow-resize)
		(begin (set! width w)
		       (set! height h)
		       (send (get-admin) resized this #f)
		       #t)))]
	[get-extent
	 (lambda (dc x y w h descent space lspace rspace)
	   (for-each (lambda (b) (when (box? b) (set-box! b 0)))
		     (list descent space lspace rspace))
	   (when (box? w) (set-box! w width))
	   (when (box? h) (set-box! h height)))])

      (private
	[x _x]
	[y _y])
      (public
	[get-x (lambda () x)]
	[get-y (lambda () y)]
	[set-x (lambda (nx) (set! x nx))]
	[set-y (lambda (ny) (set! y ny))])
      (sequence (super-init)))))
