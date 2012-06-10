#lang racket/base

  (require racket/contract
           racket/class
           racket/draw
           racket/math
           racket/gui/dynamic
           racket/snip
           "mrpict.rkt")

  ;; Utilities for use with mrpict
  
  (provide cons-colorized-picture
	   color-frame
	   round-frame
	   color-round-frame
	   color-dash-frame

	   arrow
	   arrowhead
	   arrowhead/offset
	   arrow-line
	   arrows-line
	   pip-line
	   pip-arrow-line
	   pip-arrows-line
	   
           ellipse
           filled-ellipse
	   circle
	   disk
           rectangle
           filled-rectangle
	   rounded-rectangle
           filled-rounded-rectangle
           
	   cloud
	   file-icon
	   jack-o-lantern
	   angel-wing
	   desktop-machine
	   standard-fish
           thermometer
           
	   add-line
	   add-arrow-line
	   add-arrows-line

	   bitmap-draft-mode

           find-pen
           find-brush
           
           color-series
           scale-color
	   scale/improve-new-text
           
           cellophane

	   inset/clip
           clip

	   hyperlinkize)
  
  (define (pict-path? p)
    (or (pict? p) 
        (and (pair? p)
             (list? p)
             (andmap pict? p))))
  
  (provide/contract 
   [scale (case-> (-> pict? number? number? pict?)
                  (-> pict? number? pict?))]
   [rotate (case-> (-> pict? number? pict?))]
   [pin-line (->* (pict?
                   pict-path? (-> pict? pict-path? (values number? number?))
                   pict-path? (-> pict? pict-path? (values number? number?)))
                  ((or/c false/c number?)
                   (or/c false/c string?)
                   boolean?
                   #:style (or/c false/c symbol?))
                  pict?)]
   [pin-arrow-line (->* (number?
                         pict?
                         pict-path? (-> pict? pict-path? (values number? number?))
                         pict-path? (-> pict? pict-path? (values number? number?)))
                        ((or/c false/c number?) 
                         (or/c false/c string?)
                         boolean?
                         boolean?
                         #:style (or/c false/c symbol?)
                         #:hide-arrowhead? any/c)
                        pict?)]
   [pin-arrows-line (->* (number? pict?
                           pict-path? (-> pict? pict-path? (values number? number?))
                           pict-path? (-> pict? pict-path? (values number? number?)))
                          ((or/c false/c number?)
                           (or/c false/c string?)
                           boolean?
                           boolean?
                           #:style (or/c false/c symbol?)
                           #:hide-arrowhead? any/c)
                          pict?)]
   [bitmap (-> (or/c path-string?
                     (is-a?/c bitmap%)
                     (is-a?/c image-snip%))
               pict?)]
   )



  (define (re-pict box naya)
    (let ([w (pict-width box)]
	  [h (pict-height box)]
	  [d (pict-descent box)]
	  [a (pict-ascent box)])
      (make-pict (pict-draw naya)
		 w h
		 a d
		 (list (make-child box 0 0 1 1 0 0))
		 #f
                 (pict-last box))))
  
  (define cons-colorized-picture
    (lambda (p color cmds)
      (re-pict
       p
       (cc-superimpose
	p
	(colorize
	 (cons-picture
	  (ghost (launder p))
	  cmds)
	 color)))))

  (define (round-frame p radius)
    (re-pict
     p
     (cc-superimpose
      p
      (let ([w (pict-width p)]
	    [h (pict-height p)])
	(dc (lambda (dc x y)
	      (let ([b (send dc get-brush)])
		(send dc set-brush (send the-brush-list find-or-create-brush
					 "white" 'transparent))
		(send dc draw-rounded-rectangle x y w h radius)
		(send dc set-brush b)))
	    (pict-width p) (pict-height p))))))

  ;; FIXME: abstract common part of color-frame, etc.

  (define color-frame
    (case-lambda
     [(p color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (frame (ghost (launder p))) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p color) (color-frame p color #f)]))
  
  (define color-round-frame
    (case-lambda
     [(p radius color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (round-frame (ghost (launder p)) radius) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p radius color) (color-round-frame p radius color #f)]))  

  (define color-dash-frame
    (case-lambda
     [(p seg-length color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (dash-frame (ghost (launder p)) seg-length) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p seg-length color) (color-dash-frame p seg-length color #f)]))  

  ;; Returns three values: pict dx dy
  ;;  dx is in [-size, 0] and dy is in [0, size]
  (define (generic-arrow stem? solid? size angle pen-thickness)
    (values
     (dc
      (lambda (dc x y)
	(define (pt->xform-obj p)
	  (let* ([x (car p)]
		 [y (cadr p)]
		 [d (sqrt (+ (* x x) (* y y)))]
		 [a (atan y x)])
	    (make-object point% 
              (* d size 1/2 (cos (+ a angle)))
              (* d size 1/2 (- (sin (+ a angle)))))))
	(let ([b (send dc get-brush)]
	      [p (send dc get-pen)])
	  (send dc set-pen (send the-pen-list
				 find-or-create-pen
				 (send p get-color)
				 (if solid? 0 (send p get-width))
				 'solid))
	  (send dc set-brush (send the-brush-list
				   find-or-create-brush
				   (if solid? (send p get-color) "white")
				   'solid))
	  (send dc draw-polygon 
		(map pt->xform-obj
		     (if stem?
			 `((1 0)
			   (0 -1)
			   (0 -1/2)
			   (-1 -1/2)
			   (-1 1/2)
			   (0 1/2)
			   (0 1))
			 `((1 0)
			   (-1 -1)
			   (-1/2 0)
			   (-1 1))))
		(+ x (/ size 2)) (+ y (/ size 2)))
	  (send dc set-brush b)
	  (send dc set-pen p)))
      size size)
     (- (- 0 (* 1/2 size (cos angle))) (/ size 2))
     (- (+ (* 1/2 size) (- (* 1/2 size (sin angle)))) size)))

  (define (arrow/delta size angle)
    (generic-arrow #t #t size angle 0))
  (define (arrow size angle)
    (let-values ([(p dx dy) (arrow/delta size angle)])
      p))

  (define (arrowhead/delta pen-thickness size angle solid-head?)
    (generic-arrow #f solid-head? size angle pen-thickness))
  (define (arrowhead size angle)
    (let-values ([(p dx dy) (arrowhead/delta 0 size angle #t)])
      p))
  (define (arrowhead/offset size angle)
    (arrowhead/delta 0 size angle #t))

  (define (pip-line dx dy size)
    (picture
     0 0
     `((connect 0 0 ,dx ,(- dy)))))

  (define (arrow-line dx dy size)
    (let-values ([(a adx ady) (arrowhead/delta 0 size (atan dy dx) #t)])
      (picture
       0 0
       `((connect 0 0 ,dx ,dy)
	 (place ,(+ dx adx) ,(+ ady dy) ,a)))))

  (define (pip-arrow-line dx dy size)
    (arrow-line dx (- dy) size))

  (define (arrows-line dx dy size)
    (picture
     0 0
     `((place 0 0 ,(arrow-line dx dy size))
       (place ,dx ,dy ,(arrow-line (- dx) (- dy) size)))))

  (define (pip-arrows-line dx dy size)
    (arrows-line dx (- dy) size))

  (define (filled-rectangle w h #:draw-border? [draw-border? #t])
    (dc
     (lambda (dc x y)
       (let ([b (send dc get-brush)]
             [p (send dc get-pen)])
         (send dc set-brush (send the-brush-list find-or-create-brush
                                  (send p get-color)
                                  'solid))
         (unless draw-border?
           (send dc set-pen "black" 1 'transparent))
	 (send dc draw-rectangle x y w h)
         (send dc set-brush b)
         (send dc set-pen p)))
     w
     h))
  
  (define (rectangle w h)
    (dc
     (lambda (dc x y)
       (let ([b (send dc get-brush)])
         (send dc set-brush (send the-brush-list find-or-create-brush
                                  "white" 'transparent))
         (send dc draw-rectangle x y w h)
         (send dc set-brush b)))
     w
     h))
  
  (define (rounded-rectangle w h [corner-radius -0.25] #:angle [angle 0])
    (let ([dc-path (new dc-path%)])
      (send dc-path rounded-rectangle 0 0 w h corner-radius)
      (send dc-path rotate angle)
      (let-values ([(x y w h) (send dc-path get-bounding-box)])
        (dc (λ (dc dx dy) 
              (let ([brush (send dc get-brush)])
                (send dc set-brush (send the-brush-list find-or-create-brush
                                         "white" 'transparent))
                (send dc draw-path dc-path (- dx x) (- dy y))
                (send dc set-brush brush)))
            w
            h))))
  
  (define (filled-rounded-rectangle w h [corner-radius -0.25] #:angle [angle 0] #:draw-border? [draw-border? #t])
    (let ([dc-path (new dc-path%)])
      (send dc-path rounded-rectangle 0 0 w h corner-radius)
      (send dc-path rotate angle)
      (let-values ([(x y w h) (send dc-path get-bounding-box)])
        (dc (λ (dc dx dy) 
              (let ([brush (send dc get-brush)]
                    [pen (send dc get-pen)])
                (send dc set-brush (send the-brush-list find-or-create-brush
                                         (send (send dc get-pen) get-color)
                                         'solid))
                (unless draw-border?
                  (send dc set-pen "black" 1 'transparent))
                (send dc draw-path dc-path (- dx x) (- dy y))
                (send dc set-brush brush)
                (send dc set-pen pen)))
            w
            h))))
  
  (define (circle size) (ellipse size size))
  
  (define (ellipse width height)
    (dc (lambda (dc x y)
	  (let ([b (send dc get-brush)])
	    (send dc set-brush (send the-brush-list find-or-create-brush
				     "white" 'transparent))
	    (send dc draw-ellipse x y width height)
	    (send dc set-brush b)))
	width height))

  (define (disk size #:draw-border? [draw-border? #t])
    (filled-ellipse size size #:draw-border? draw-border?))
  
  (define (filled-ellipse width height #:draw-border? [draw-border? #t])
    (dc (lambda (dc x y)
	  (define b (send dc get-brush))
          (define p (send dc get-pen))
          (send dc set-brush (send the-brush-list find-or-create-brush
                                   (send (send dc get-pen) get-color)
                                   'solid))
          (unless draw-border?
            (send dc set-pen "black" 1 'transparent))
          (send dc draw-ellipse x y width height)
          (send dc set-brush b)
          (send dc set-pen p))
	width height))

  (define cloud
    (case-lambda
     [(w h) (cloud w h "gray")]
     [(w h color)
      (dc
       (lambda (dc x y)
	 (let ([b (send dc get-brush)]
	       [p (send dc get-pen)])
	   (send dc set-pen (send the-pen-list
				  find-or-create-pen
				  "white" 0 'transparent))
	   (send dc set-brush (send the-brush-list
				    find-or-create-brush
				    color
				    'solid))
	   (send dc draw-ellipse
		 x (+ y (* 1/4 h))
		 (* 1/2 w) (* 1/2 h))
	   (send dc draw-ellipse
		 (+ x (* 1/5 w)) y
		 (* 3/5 w) (add1 (* 2/5 h)))
	   (send dc draw-ellipse
		 (+ x (* 1/5 w)) (+ y (* 1/3 h))
		 (* 3/5 w) (* 2/3 h))
	   (send dc draw-ellipse
		 (+ x (* 3/5 w)) (+ y (* 1/4 h))
		 (* 2/5 w) (* 1/3 h))
	   (send dc draw-ellipse
		 (+ x (* 3/5 w)) (+ y (* 1/2 h))
		 (* 2/5 w) (* 1/3 h))

	   (send dc set-brush b)
	   (send dc set-pen p)))
       w h)]))
  
  (define (thermometer #:height-% [height-% 1]
                       #:color-% [color-% height-%]
                       #:ticks [ticks 4]
                       #:start-color [_start-color "lightblue"]
                       #:end-color [_end-color "lightcoral"]
                       #:top-circle-diameter [top-circle-diameter 40]
                       #:bottom-circle-diameter [bottom-circle-diameter 80]
                       #:stem-height [stem-height 180]
                       #:mercury-inset [mercury-inset 8])
    (define (to-color s)
      (if (string? s)
          (or (send the-color-database find-color s) 
              (send the-color-database find-color "white"))
          s))
    (define start-color (to-color _start-color))
    (define end-color (to-color _end-color))
    (define (between lo hi) (round (+ lo (* (- hi lo) color-%))))
    (define fill-color (make-object color%
                         (between (send start-color red) (send end-color red))
                         (between (send start-color green) (send end-color green))
                         (between (send start-color blue) (send end-color blue))))
    (define tw bottom-circle-diameter)
    (define th
      (+ stem-height 
         (/ top-circle-diameter 2)
         (/ bottom-circle-diameter 2)))
    
    (define (make-region dc dx dy offset %)
      (define top (new region% [dc dc]))
      (define bottom (new region% [dc dc]))
      (define middle (new region% [dc dc]))
      (send top set-ellipse 
            (+ dx 
               (/ (- bottom-circle-diameter top-circle-diameter) 2)
               offset)
            (+ dy offset (* (- 1 %) stem-height))
            (- top-circle-diameter offset offset)
            (- top-circle-diameter offset offset))
      (send middle set-rectangle 
            (+ dx 
               (/ (- bottom-circle-diameter top-circle-diameter) 2)
               offset)
            (+ dy (/ top-circle-diameter 2) (* (- 1 %) stem-height))
            (- top-circle-diameter offset offset)
            (* % stem-height))
      (send bottom set-ellipse 
            (+ dx offset)
            (+ dy (+ (/ top-circle-diameter 2) 
                     stem-height
                     (- (/ bottom-circle-diameter 2)))
               offset)
            (- bottom-circle-diameter offset offset)
            (- bottom-circle-diameter offset offset))
      (send top union middle)
      (send top union bottom)
      top)
    
    (dc
     (λ (dc dx dy)
       (define old-pen (send dc get-pen))
       (define old-brush (send dc get-brush))
       (define boundary (make-region dc dx dy 0 1))
       (define fill (make-region dc dx dy mercury-inset height-%))
       (define old-rgn (send dc get-clipping-region))
       (send dc set-clipping-region boundary)
       (send dc set-brush "black" 'solid)
       (send dc draw-rectangle dx dy tw th)
       (send dc set-clipping-region fill)
       (send dc set-brush fill-color 'solid)
       (send dc draw-rectangle dx dy tw th)
       (send dc set-pen "black" mercury-inset 'solid)
       (for ([x (in-range ticks)])
         (define y (+ (/ top-circle-diameter 2)
                      (* (/ (+ x 1/2) ticks) 
                         (- stem-height (/ bottom-circle-diameter 3)))))
         (send dc draw-line 
               dx
               (+ dy y)
               (+ dx (/ tw 2))
               (+ dy y)))
       
       (send dc set-clipping-region old-rgn)
       (send dc set-brush old-brush)
       (send dc set-pen old-pen))
     tw th))

  (define file-icon
    (lambda (w h gray [fancy? #f])
      (dc
       (let* ([sw (lambda (x) (* (/ w 110) x))]
	      [sh (lambda (y) (* (/ h 150) y))]
	      [->pt (lambda (l)
		      (map (lambda (p)
			     (make-object point% 
					  (sw (car p))
					  (sh (cadr p))))
			   l))])
	 (lambda (dc x y)
	   (define p (send dc get-pen))
	   (define b (send dc get-brush))

	   (let* ([bg-color (cond
                                 [(or (string? gray) (is-a? gray color%)) gray]
                                 [gray (make-object color% 200 200 255)]
                                 [else "white"])]
                  [line-color (if fancy?
                                  (scale-color 0.6 bg-color)
                                  "black")]
                  [color (send the-brush-list
                               find-or-create-brush
                               bg-color
                               'solid)])

	     (send dc set-pen (send the-pen-list 
				    find-or-create-pen 
                                    line-color
				    (send p get-width)
				    'solid))
	     (send dc set-brush color)
	     
	     (send dc draw-polygon 
		   (->pt '((0 0)
			   (0 150)
			   (110 150)
			   (110 20)
			   (90 0)))
		   x y)

             (send dc draw-line (+ x (sw 90)) (+ y 1) (+ x (sw 90)) (+ y (sh 20)))
             (send dc draw-line (+ x (sw 90)) (+ y (sh 20)) (+ x (sw 110) -1) (+ y (sh 20))))
	   
           (send dc set-brush b)
	   (send dc set-pen p)))
       w h)))

  (define angel-wing
    (lambda (w h left?)
      (dc
       (lambda (dc x y)
	 (let-values ([(sx sy) (send dc get-scale)]
		      [(dx dy) (send dc get-origin)])
	   (let ([nsx (* sx (/ w 54))]
		 [nsy (* sy (/ h 110))])
	     (send dc set-origin (+ dx (* x sx) (* (- 16) nsx)) (+ dy (* y sy) (* (- 20) nsy)))
	     (send dc set-scale nsx nsy)

	     (let ([wing
		    (list
		     
		     (list 70 (+ 50 40)  35 65     20 20)
		     (list 20 20    (- 20 5) (+ 20 30)    (+ 20 5) (+ 20 60))
		     (list (+ 20 5) (+ 20 60)    50 100    70 (+ 50 45))
		     
		     (list 22 70   (- 30 5) (+ 65 30)   (+ 30 5) (+ 65 40))
		     (list (+ 30 5) (+ 65 40)  50 110     70 (+ 50 50))
		     
		     (list 32 102   (- 40 5) (+ 65 50)   (+ 40 5) (+ 65 58))
		     (list (+ 40 5) (+ 65 58)   60  130    70 (+ 50 52)))])
	       (when left?
		 (for-each
		  (lambda (spline)
		    (send dc draw-spline . spline))
		  wing))
	       (unless left?
		 (for-each
		  (lambda (spline)
		    (let-values ([(x1 y1 x2 y2 x3 y3) (apply values spline)])
		      (send dc draw-spline (- 87 x1) y1 (- 86 x2) y2 (- 86 x3) y3)))
		  wing)))

	     (send dc set-origin dx dy)
	     (send dc set-scale sx sy))))
       w h)))

  (define desktop-machine
    (lambda (s [style null])
      (define icon
	(let ([bm (if (and (list? style) (memq 'plt style))
		      (make-object bitmap% (build-path (collection-path "icons") "plt-small-shield.gif"))
		      #f)])
	  (dc (lambda (dc x y)
		(let-values ([(sx sy) (send dc get-scale)]
			     [(dx dy) (send dc get-origin)]
			     [(op) (send dc get-pen)]
			     [(ob) (send dc get-brush)])
		  (send dc set-origin (+ dx (* sx x) (* s sx 10)) (+ dy (* sy y) (* s sy 15)))
		  (send dc set-scale (* sx s) (* sy s))
		  
		  (let ([gray (send the-brush-list
				    find-or-create-brush
				    "gray"
				    'solid)])
		    (send dc set-brush gray)
		    (send dc draw-polygon (list
					   (make-object point% 10 60)
					   (make-object point% 0 80)
					   (make-object point% 80 80)
					   (make-object point% 100 60)
					   (make-object point% 100 0)
					   (make-object point% 20 0)
					   (make-object point% 10 5))))
		  (send dc draw-line 80 80 90 60)
		  (send dc draw-rectangle 10 5 80 55)
		  (send dc set-brush (send the-brush-list
					   find-or-create-brush
					   "white"
					   'solid))
		  (send dc draw-rounded-rectangle 15 10 70 45 5)

		  (when (and (list? style) 
			     (or (memq 'devil style)
				 (memq 'binary style)))
		    (send dc set-font (make-object font% 12 'modern 'normal 'normal))
		    (let-values ([(w h d a) (send dc get-text-extent "101010")])
		      (let ([dx (+ (/ (- 70 w) 2) 15)]
			    [dy (+ (/ (- 45 (* 2 h) 2) 2) 10)])
			(send dc draw-text "101010" dx dy)
			(send dc draw-text "010101" dx (+ dy h 2))))
		    
		    (when (memq 'devil style)
		      (send dc set-brush (send the-brush-list
					       find-or-create-brush
					       "red"
					       'solid))
		      (let ([horn (list
				   (make-object point% 0 17)
				   (make-object point% 2 0)
				   (make-object point% 4 17))])
			(send dc draw-polygon horn 30 -15)
			(send dc draw-polygon horn 70 -15))
		      
		      (send dc draw-polygon (list
					     (make-object point% 0 0)
					     (make-object point% 10 2)
					     (make-object point% 0 6))
			    115 32)
		      
		      (send dc set-pen (send the-pen-list
					     find-or-create-pen
					     "red"
					     2
					     'solid))
		      (send dc draw-line 101 55 110 55)
		      (send dc draw-spline 110 55   130 50    110  45)
		      (send dc draw-spline 110 45   90 40    115  35)))
		    
		  (send dc set-origin dx dy)

		  (send dc set-pen op)
		  (send dc set-brush ob)

		  (send dc set-scale (* sx s 2/3) (* sy s 2/3))

		  (when (and (list? style) (memq 'plt style))
		    (when (send bm ok?)
		      (let ([w (send bm get-width)]
			    [h (send bm get-height)])
			(send dc draw-bitmap bm 
			      (/ (+ x (/ (- (* s 70) (* w 2/3 s)) 2) (* s 25)) (* 2/3 s))
			      (/ (+ y (/ (- (* s 45) (* h 2/3 s)) 2) (* s 25)) (* 2/3 s))))))

		  (send dc set-scale sx sy)))
	      (* s 120) (* s 115))))
      (if (pict? style)
	  (lt-superimpose
	   icon
	   (inset
	    (cc-superimpose (blank (* s 70) (* s 45)) style)
	    (* s 25) (* s 25) 0 0))
	  icon)))

  (define jack-o-lantern
    (lambda (size [pumpkin-color "orange"] [face-color "black"] [stem-color "brown"])
      (dc (lambda (dc x y)
	    (let ([b (send dc get-brush)]
		  [p (send dc get-pen)]
		  [set-brush (lambda (c)
			       (send dc set-brush 
				     (send the-brush-list
					   find-or-create-brush
					   c 'solid)))]
		  [r (make-object region% dc)]
		  [path (make-object dc-path%)]
		  [c (send dc get-clipping-region)])
	      (send dc set-pen (send the-pen-list
				     find-or-create-pen
				     "white" 1 'transparent))

	      ;; Stem ----------------------------------------
	      (send path arc
		    (+ x (* 0.42 size)) (- y (*  0.2 size))
		    size size
		    (* 0.8 pi) pi)
	      (send path arc
		    (+ x (* 0.52 size)) (- y (* 0.1 size))
		    (* 0.8 size) (* 0.8 size)
		    pi (* 0.8 pi) #f)
	      (send r set-path path)
	      (send dc set-clipping-region r)
	      (set-brush stem-color)
	      (send dc draw-rectangle x y size size)

	      ;; Body ----------------------------------------
	      (send dc set-clipping-region c)
	      (set-brush pumpkin-color)

	      (send dc draw-ellipse 
		    x (+ y (* 0.2 size))
		    (* 0.4 size) (* 0.8 size))
	      (send dc draw-ellipse 
		    (+ x (* 0.6 size)) (+ y (* 0.2 size))
		    (* 0.4 size) (* 0.8 size))

	      (send dc draw-ellipse 
		    (+ x (* 0.2 size)) (+ y (* 0.15 size))
		    (* 0.4 size) (* 0.9 size))
	      (send dc draw-ellipse 
		    (+ x (* 0.4 size)) (+ y (* 0.15 size))
		    (* 0.4 size) (* 0.9 size))

	      ;; Smile ----------------------------------------

	      (send r set-rectangle x (+ y (* 0.4 size)) size (* 0.7 size))
	      (send dc set-clipping-region r)

	      (set-brush face-color)
	      (send dc draw-ellipse
		    (+ x (* 0.15 size)) (+ y (* 0.2 size))
		    (* 0.7 size) (* 0.7 size))
	      
	      (set-brush pumpkin-color)
	      (send dc draw-ellipse
		    (+ x (* 0.15 size)) (sub1 (+ y (* 0.2 size)))
		    (* 0.7 size) (* 0.5 size))
	      (send dc draw-rectangle
		    (+ x (* 0.35 size)) (+ y (* 0.55 size))
		    (* 0.1 size) (* 0.2 size))
	      
	      ;; Eyes ----------------------------------------
	      (send dc set-clipping-region c)
	      (set-brush face-color)

	      (send dc draw-ellipse
		    (+ x (* 0.25 size)) (+ y (* 0.3 size))
		    (* 0.175 size) (* 0.25 size))
	      (send dc draw-ellipse
		    (+ x (* (- 0.75 0.175) size)) (+ y (* 0.3 size))
		    (* 0.175 size) (* 0.25 size))

	      (set-brush pumpkin-color)

	      (send dc draw-polygon
		    (list
		     (make-object point%
				  (* 0.5 size)
				  (* 0.45 size))
		     (make-object point%
				  (* 0.2 size)
				  (* 0.25 size))
		     (make-object point%
				  (* 0.8 size)
				  (* 0.25 size)))
		    x y)
	      
	      (send dc set-brush b)
	      (send dc set-pen p)))
	  size (* 1.1 size))))

  (define standard-fish 
    (lambda (w h [direction 'left] [c "blue"] [ec #f] [mouth-open #f])
      (define no-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
      (define color (if (string? c) (make-object color% c) c))
      (define dark-color (scale-color 0.8 color))
      (define eye-color (and ec (if (string? ec) (make-object color% ec) ec)))
      (define dark-eye-color color)
      (define mouth-open? (and mouth-open
                               (or (not (number? mouth-open))
                                   (not (zero? mouth-open)))))
      (define mouth-open-amt (if (number? mouth-open)
                                 mouth-open
                                 (if mouth-open 1.0 0.0)))
      (dc (lambda (dc x y)
            (let ([rgn (make-object region% dc)]
                  [old-rgn (send dc get-clipping-region)]
                  [old-pen (send dc get-pen)]
                  [old-brush (send dc get-brush)]
                  [flip-rel (lambda (x0) 
                              (if (eq? direction 'left)
                                  x0
                                  (- w x0)))]
                  [flip (lambda (x0 w0) 
                          (if (eq? direction 'left)
                              x0
                              (+ x (- w (- x0 x) w0))))]
		  [set-rgn (lambda (rgn flip? old-rgn)
                             (let ([dy (if flip? (/ h 2) 0)]
                                   [wf (λ (x) (* (if (eq? 'left direction) x (+ 1 (* x -1))) w))])
			       (if mouth-open?
				   (send rgn set-polygon
					 (list (make-object point% (wf 0) dy)
					       (make-object point% (wf 1) dy)
					       (make-object point% (wf 1) (- (* 1/2 h) dy))
					       (make-object point% (wf 1/6) (- (* 1/2 h) dy))
                                               (make-object point% (wf 0) (if flip?
                                                                              (* 1/6 mouth-open-amt h)
                                                                              (+ (* 1/3 h)
                                                                                 (* 1/6 (- 1 mouth-open-amt) h)))))
					 x (+ y dy))
				   (send rgn set-rectangle 
					 x (+ y dy)
					 w (/ h 2))))
                             (when old-rgn
                               (send rgn intersect old-rgn)))])
              (send dc set-pen no-pen)
              (color-series
               dc 4 1
               dark-color color
               (lambda (ii)
		 (define i (* ii (min 1 (* w 1/100))))
                 (send dc draw-polygon (list (make-object point% (flip-rel (+ (* 1/2 w) i)) (* 1/10 h))
                                             (make-object point% (flip-rel (- (* 3/4 w) i)) (+ 0 i))
                                             (make-object point% (flip-rel (- (* 3/4 w) i)) (- (* 2/10 h) i)))
                       x y)
                 (send dc draw-polygon (list (make-object point% (flip-rel (+ (* 1/2 w) i)) (* 9/10 h))
                                             (make-object point% (flip-rel (- (* 3/4 w) i)) (- h i))
                                             (make-object point% (flip-rel (- (* 3/4 w) i)) (+ (* 8/10 h) i)))
                       x y)
                 (send dc draw-polygon (list (make-object point% (flip-rel (+ (* 3/4 w) i)) (/ h 2))
                                             (make-object point% (flip-rel (- w i)) (+ (* 1/10 h) i))
                                             (make-object point% (flip-rel (- w i)) (- (* 9/10 h) i)))
                       x y))
               #f #t)

	      (set-rgn rgn #f old-rgn)
	      (send dc set-clipping-region rgn)
              (color-series
               dc 4 1
               dark-color color
               (lambda (i)
                 (send dc draw-ellipse (+ (- x (* 1/4 w)) i) (+ y i)
                       (- (* 6/4 w) (* 2 i)) (- (* 4 h) (* 2 i))))
               #f #t)
              (send dc set-clipping-region old-rgn)

              (set-rgn rgn #t old-rgn)
              (send dc set-clipping-region rgn)
              (color-series
               dc 4 1
               dark-color color
               (lambda (i)
                 (send dc draw-ellipse (+ (- x (* 1/4 w)) i) (+ (- y (* 3 h)) i)
                       (- (* 6/4 w) (* 2 i)) (- (* 4 h) (* 2 i))))
               #f #t)
              (send dc set-clipping-region old-rgn)

              (when mouth-open?
                ;; Repaint border, just in case round-off does weird things
                (send dc set-pen color 1 'solid)
                (let ([y (+ y (/ h 2))])
                  (send dc draw-line 
                        (+ x (if (eq? direction 'left) (* 1/6 w) 6)) y
                        (+ x (if (eq? direction 'left) w (* 5/6 w)) -6) y))
                (send dc set-pen no-pen))

              (color-series
               dc 4 1
               dark-color color
               (lambda (ii)
		 (define i (* ii (min 1 (* w 1/100))))
		 (send dc draw-polygon (list (make-object point% (flip-rel (+ (* 1/2 w) i)) (/ h 2))
					     (make-object point% (flip-rel (- (* 5/8 w) i)) (+ (* 1/4 h) i))
					     (make-object point% (flip-rel (- (* 5/8 w) i)) (- (* 3/4 h) i)))
		       x y))
               #f #t)
              (when eye-color
		(if (eq? eye-color 'x)
		    (begin
		      (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
		      (let* ([ew (* 1/10 w)]
			     [eh (* 1/10 h)]
			     [x0 (flip (+ x (* 1/5 w)) ew)]
			     [x1 (flip (+ x (* 1/5 w) ew) ew)]
			     [y0 (+ y (* 2/3 h))]
			     [y1 (- (+ y (* 2/3 h)) eh)])
			(send dc draw-line x0 y0 x1 y1)
			(send dc draw-line x0 y1 x1 y0))
		      )
		    (color-series
		     dc
		     1/20 1/80
		     dark-eye-color eye-color
		     (lambda (s)
		       (let ([ew (* (- 1/10 s) w)])
			 (send dc draw-ellipse 
			       (flip (+ x (* 1/5 w) (* s 1/2 w)) ew)
			       (+ y (* 1/3 h) (* (* s 4/2) 1/2 h))
			       ew
			       (* (- 1/10 s) 4/2 h))))
		     #f #t)))
              (send dc set-pen old-pen)
              (send dc set-brush old-brush)))
          w h)))

  (define (-add-line base src find-src dest find-dest thickness color arrow-size arrow2-size under? solid-head?
                     #:style [style #f]
                     #:hide-arrowhead? [hide-arrowhead? #f])
    (let-values ([(sx sy) (find-src base src)]
                 [(dx dy) (find-dest base dest)])
      (let ([arrows
             (let ([p (cons-picture
                       (ghost (launder base))
                       `(,(let* ([angle (atan (- sy dy) 
                                              (- sx dx))]
                                 [cosa (cos angle)]
                                 [sina (sin angle)]
                                 ;; If there's an arrow, line goes only half-way in
                                 [ddx (* (or arrow-size 0) 0.5 cosa)]
                                 [ddy (* (or arrow-size 0) 0.5 sina)]
                                 [dsx (* (or arrow2-size 0) 0.5 (- cosa))]
                                 [dsy (* (or arrow2-size 0) 0.5 (- sina))])
                            `(connect ,(+ sx dsx) ,(+ sy dsy) ,(+ dx ddx) ,(+ dy ddy)))
                         ,@(if (and arrow-size
                                    (not hide-arrowhead?))
                               (let-values ([(arrow xo yo)
                                             (arrowhead/delta
                                              (or thickness 0)
                                              arrow-size 
                                              (atan (- dy sy) 
                                                    (- dx sx))
                                              solid-head?)])
                                 `((place ,(+ dx xo) ,(+ dy yo) ,arrow)))
                               null)
                         ,@(if (and arrow2-size
                                    (not hide-arrowhead?))
                               (let-values ([(arrow xo yo)
                                             (arrowhead/delta
                                              (or thickness 0)
                                              arrow-size 
                                              (atan (- sy dy) 
                                                    (- sx dx))
                                              solid-head?)])
                                 `((place ,(+ sx xo) ,(+ sy yo) ,arrow)))
                               null)))])
               (let* ([p2 (if thickness
                              (linewidth thickness p)
                              p)]
                      [p2 (if style
                              (linestyle style p2)
                              p2)])
                 (if color
                     (colorize p2 color)
                     p2)))])
        (refocus
         (if under?
             (cc-superimpose arrows base)
             (cc-superimpose base arrows))
         base))))

  (define add-line
    (lambda (base src find-src dest find-dest [thickness #f] [color #f] [under? #f])
      (-add-line base src find-src dest find-dest thickness color #f #f under? #t)))
  
  (define add-arrow-line
    (lambda (arrow-size base src find-src dest find-dest [thickness #f] [color #f] [under? #f]
                        #:hide-arrowhead? [hide-arrowhead? #f])
      (-add-line base src find-src dest find-dest thickness color arrow-size #f under? #t
                 #:hide-arrowhead? hide-arrowhead?)))
  
  (define add-arrows-line
    (lambda (arrow-size base src find-src dest find-dest [thickness #f] [color #f] [under? #f]
                        #:hide-arrowhead? [hide-arrowhead? #f])
      (-add-line base src find-src dest find-dest thickness color arrow-size arrow-size under? #t
                 #:hide-arrowhead? hide-arrowhead?)))

  (define (flip-find-y find-)
    (lambda (base path)
      (let-values ([(x y) (find- base path)])
	(values x (- (pict-height base) y)))))

  (define pin-line
    (lambda (base src find-src dest find-dest [thickness #f] [color #f] [under? #f] #:style [style #f])
      (-add-line base src (flip-find-y find-src) dest (flip-find-y find-dest)
                 thickness color #f #f under? #t
                 #:style style)))

  (define pin-arrow-line
    (lambda (arrow-size base src find-src dest find-dest [thickness #f] [color #f] [under? #f] [solid-head? #t]
                        #:hide-arrowhead? [hide-arrowhead? #f]
                        #:style [style #f])
      (-add-line base src (flip-find-y find-src) dest (flip-find-y find-dest)
		 thickness color arrow-size #f under? solid-head?
                 #:style style
                 #:hide-arrowhead? hide-arrowhead?)))
  
  (define pin-arrows-line
    (lambda (arrow-size base src find-src dest find-dest [thickness #f] [color #f] [under? #f] [solid-head? #t]
                        #:hide-arrowhead? [hide-arrowhead? #f]
                        #:style [style #f])
      (-add-line base src (flip-find-y find-src) dest (flip-find-y find-dest)
                 thickness color arrow-size arrow-size under? solid-head?
                 #:style style
                 #:hide-arrowhead? hide-arrowhead?)))
  
  (define black-color (make-object color% 0 0 0))

  (define bitmap-draft-mode (make-parameter #f (lambda (x) (and x #t))))

  (define (bitmap filename)
    (let ([bm (cond
	       [(bitmap-draft-mode) #f]
	       [(filename . is-a? . bitmap%) filename]
	       [(path-string? filename) (make-object bitmap% filename 'unknown/mask)]
	       [(and (gui-available?)
                     (filename . is-a? . (gui-dynamic-require 'image-snip%)))
                (send filename get-bitmap)])])
      (if (and bm (send bm ok?))
	  (let ([w (send bm get-width)]
		[h (send bm get-height)])
	    (dc
	     (lambda (dc x y)
	       (send dc draw-bitmap bm x y 'solid black-color (send bm get-loaded-mask)))
	     w h))
	  (frame (inset (colorize (text "bitmap failed") "red") 2)))))
  
  (define find-brush
    (lambda (color [style 'solid])
      (send the-brush-list find-or-create-brush color style)))
  (define find-pen
    (lambda (color [size 1] [style 'solid])
      (send the-pen-list find-or-create-pen color size style)))  

  (define (color-series dc steps dstep start-c end-c f pen? brush?)
    (let ([start-c (if (string? start-c)
		       (make-object color% start-c)
		       start-c)]
	  [end-c (if (string? end-c)
		       (make-object color% end-c)
		       end-c)])
      (let ([sr (send start-c red)]
	    [sg (send start-c green)]
	    [sb (send start-c blue)]
	    [er (send end-c red)]
	    [eg (send end-c green)]
	    [eb (send end-c blue)]
	    [c (make-object color%)]
	    [s (lambda (start end i)
                 (if (= steps 0)
                     end
                     (floor (+ start (* (- end start) (/ i steps))))))])
	(let loop ([i 0])
	  (send c set (s sr er i) (s sg eg i) (s sb eb i))
	  (when brush?
	    (send dc set-brush (find-brush c)))
	  (when pen?
	    (send dc set-pen (find-pen c)))
	  (f i)
	  (unless (= i steps)
	    (loop (+ dstep i)))))))
  
  (define (scale-color s c)
    (let ([c (if (string? c)
                 (make-object color% c)
                 c)])
      (let ([s (lambda (v)
                 (if (> s 1)
                     (- 255 (inexact->exact (floor (/ (- 255 v) s))))
                     (min 255 (inexact->exact (floor (* v s))))))])
        (make-object color%
          (s (send c red))
          (s (send c green))
          (s (send c blue))))))
  
  (define scale
    (case-lambda
     [(p x-factor y-factor)
      (let ([drawer (make-pict-drawer p)])
	(let ([new
	       (dc
		(lambda (dc x y)
		  (let-values ([(xs ys) (send dc get-scale)])
		    (send dc set-scale (* xs x-factor) (* ys y-factor))
		    (drawer dc
			    (/ x x-factor)
			    (/ y y-factor))
		    (send dc set-scale xs ys)))
		(* (pict-width p) x-factor)
		(* (pict-height p) y-factor)
		(* (pict-ascent p) y-factor)
		(* (pict-descent p) y-factor))])
	  (make-pict (pict-draw new)
		     (pict-width new)
		     (pict-height new)
		     (pict-ascent new)
		     (pict-descent new)
		     (list (make-child p 0 0 x-factor y-factor 0 0))
		     #f
                     (pict-last p))))]
     [(p factor) (scale p factor factor)]))

  (define (rotate p theta)
    (let ([w (pict-width p)]
          [h (pict-height p)]
          [drawer (make-pict-drawer p)])
      (let ([dl (min 0 (* w (cos theta)) (* h (sin theta)) (+ (* w (cos theta)) (* h (sin theta))))]
            [dr (max 0 (* w (cos theta)) (* h (sin theta)) (+ (* w (cos theta)) (* h (sin theta))))]
            [dt (min 0 (* w -1 (sin theta)) (* h (cos theta)) (+ (* w -1 (sin theta)) (* h (cos theta))))]
            [db (max 0 (* w -1 (sin theta)) (* h (cos theta)) (+ (* w -1 (sin theta)) (* h (cos theta))))]
            [da (- (* (pict-ascent p) (cos theta)) (* (sin theta) w 1/2))]
            [dd (- (* (- (pict-height p) (pict-descent p)) (cos theta)) (* (sin theta) w 1/2))])
        (let ([new (dc
                    (lambda (dc x y)
                      (let ([t (send dc get-transformation)])
                        (send dc translate (- x dl) (- y dt))
                        (send dc rotate theta)
                        (drawer dc 0 0)
                        (send dc set-transformation t)))
                    (- dr dl) (- db dt) 
                    (min (- da dt) (- (- db dt) (- db dd)))
                    (min (- db da) (- db dd)))])
          (make-pict (pict-draw new)
		     (pict-width new)
		     (pict-height new)
		     (pict-ascent new)
		     (pict-descent new)
		     (list (make-child p 
                                       (- (* h (sin theta)) dl) 
                                       (max 0 (- db (* h (cos theta))))
                                       (cos theta) (cos theta) 
                                       (sin theta) (- (sin theta))))
		     #f
                     (pict-last p))))))

  (define cellophane
    (case-lambda
     [(p alpha-factor)
      (cond
       [(= 1.0 alpha-factor)
        (inset p 0)]
       [(zero? alpha-factor)
        (ghost p)]
       [else
        (let ([drawer (make-pict-drawer p)])
          (let ([new
                 (dc
                  (lambda (dc x y)
                    (let ([a (send dc get-alpha)])
                      (send dc set-alpha (* a alpha-factor))
                      (drawer dc x y)
                      (send dc set-alpha a)))
                  (pict-width p)
                  (pict-height p)
                  (pict-ascent p)
                  (pict-descent p))])
            (make-pict (pict-draw new)
                       (pict-width new)
                       (pict-height new)
                       (pict-ascent new)
                       (pict-descent new)
                       (list (make-child p 0 0 1 1 0 0))
                       #f
                       (pict-last p))))])]))

  (define inset/clip
    (case-lambda
     [(p l t r b)
      (let* ([p (inset p l t r b)]
	     [drawer (make-pict-drawer p)]
	     [w (pict-width p)]
	     [h (pict-height p)])
	(let ([new
	       (dc
		(lambda (dc x y)
		  (let ([rgn (make-object region% dc)])
		    (send rgn set-rectangle x y w h)
		    (let ([r (send dc get-clipping-region)])
		      (when r
			(send rgn intersect r))
		      (send dc set-clipping-region rgn)
		      (drawer dc x y)
		      (send dc set-clipping-region r))))
		w h (pict-ascent p) (pict-descent p))])
	  (make-pict (pict-draw new)
		     (pict-width new)
		     (pict-height new)
		     (pict-ascent new)
		     (pict-descent new)
		     (list (make-child p 0 0 1 1 0 0))
		     #f
                     (pict-last p))))]
     [(p h v) (inset/clip p h v h v)]
     [(p a) (inset/clip p a a a a)]))
  
  (define (clip p) (inset/clip p 0))
  
  (define-syntax scale/improve-new-text
    (syntax-rules ()
      [(_ expr s)
       (scale/improve-new-text expr s s)]
      [(_ expr sx sy)
       (let ([xs sx]
	     [ys sy])
	 (parameterize ([current-expected-text-scale
			 (let ([s (current-expected-text-scale)])
			   (list (* xs (car s)) (* ys (cadr s))))])
	   (scale expr xs ys)))]))

  (define (hyperlinkize r)
    (colorize (inset
	       (place-over r
			   0 (pict-height r)
			   (linewidth 2 (hline (pict-width r) 1)))
	       0 0 0 2)
	      "blue"))
  
  
  (provide/contract [explode-star
                     (-> number? number?  number? number? (or/c (is-a?/c color%) string?) pict?)])
  ;; abstract-explosion number number number number color -> pict
  (define (explode-star small-rad large-rad points line-size line-color)
    (define (find-xy radius theta)
      (values (* radius (cos theta))
              (* radius (sin theta))))
    (let ([roff (floor (/ large-rad 2))]
          [fx #f]
          [fy #f])
      (dc
       (lambda (dc dx dy)
         (let ([old-pen (send dc get-pen)])
           (send dc set-pen (send the-pen-list find-or-create-pen line-color line-size 'solid))
           (let loop ([i points]
                      [lx #f]
                      [ly #f])
             (cond
               [(zero? i) (when (and lx ly)
                            (send dc draw-line
                                  (+ dx large-rad lx)
                                  (+ dy large-rad ly)
                                  (+ dx large-rad fx)
                                  (+ dy large-rad fy)))]
               [else (let* ([this-p (- i 1)]
                            [theta1 (* 2 pi (/ this-p points))]
                            [theta2 (* 2 pi (/ (- this-p 1/2) points))])
                       (let-values ([(x1 y1) (find-xy small-rad theta1)]
                                    [(x2 y2) (find-xy large-rad theta2)])
                         (unless (and fx fy)
                           (set! fx x1)
                           (set! fy y1))
                         (when (and lx ly)
                           (send dc draw-line
                                 (+ dx large-rad lx)
                                 (+ dy large-rad ly)
                                 (+ dx large-rad x1)
                                 (+ dy large-rad y1)))
                         (send dc draw-line
                               (+ dx large-rad x1)
                               (+ dy large-rad y1)
                               (+ dx large-rad x2)
                               (+ dy large-rad y2))
                         (loop (- i 1)
                               x2
                               y2)))]))
           (send dc set-pen old-pen)))
       (* large-rad 2)
       (* large-rad 2)
       0
       0)))
