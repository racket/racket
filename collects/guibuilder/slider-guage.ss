
(module slider-guage mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss"
	   "base.ss"
	   "feature.ss")
    
  (define gb:make-slider-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field vertical-layout?)
	(inherit get-label get-callback-names gb-need-recalc-size)
	(field
	  [init-value 0]
	  [min-value 0]
	  [max-value 10]
	  [arrow-size 10]
	  [height arrow-size]
	  [line-height 3]
	  [min-width 50]
	  [darrow (list (make-object mred:point% 0 0)
			(make-object mred:point% arrow-size 0)
			(make-object mred:point% (quotient arrow-size 2) (quotient arrow-size 2)))]
	  [rarrow (list (make-object mred:point% 0 0)
			(make-object mred:point% 0 arrow-size)
			(make-object mred:point% (quotient arrow-size 2) (quotient arrow-size 2)))])
	(public*
	  [slider-install
	   (lambda (mn mx in)
	     (set! min-value mn)
	     (set! max-value mx)
	     (set! init-value in))])
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (super-new)
	       (private-field
		 [min-val (make-number-control controls "Minimum:" 0 (lambda () -10000) (lambda () 10000) 
					       (lambda (x) 
						 (set! min-value x) 
						 (send max-val check)
						 (send init-val check)
						 (gb-need-recalc-size)))]
		 [max-val (make-number-control controls "Maximum:" 10 (lambda () (send min-val get-val)) (lambda () 10000)
					       (lambda (x)
						 (set! max-value x)
						 (send init-val check)
						 (gb-need-recalc-size)))]
		 [init-val (make-number-control controls "Initial:" 0 (lambda () (send min-val get-val)) 
						(lambda () (send max-val get-val))
						(lambda (x)
						  (set! init-value x)
						  (gb-need-recalc-size)))])))]
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "slider"))]
	  [init-vertical-layout? (lambda () #f)]
	  [get-min-body-size
	   (lambda (dc)
	     (if vertical-layout?
		 (values height min-width)
		 (values min-width height)))]
	  [draw-body
	   (lambda (dc x y w h)
	     (let ([percent (/ (- init-value min-value) (- max-value min-value))])
	       (if vertical-layout?
		   (begin
		     (send dc draw-rectangle 
			   (+ x (/ arrow-size 2)) (+ y (/ arrow-size 2))
			   line-height (- h arrow-size))
		     (send dc draw-polygon rarrow x (+ y (* percent (- h arrow-size)))))
		   (begin
		     (send dc draw-rectangle 
			   (+ x (/ arrow-size 2)) (+ y (/ arrow-size 2)) 
			   (- w arrow-size) line-height)
		     (send dc draw-polygon darrow (+ x (* percent (- w arrow-size))) y)))))]
	  [gb-get-default-class (lambda () 'slider%)]
	  [gb-instantiate-arguments
	   (lambda ()
	     (list*
	      `[min-value ,min-value]
	      `[max-value ,max-value]
	      `[init-value ,init-value]
	      (super gb-instantiate-arguments)))]
	  
	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o slider-install min-value max-value init-value)
	       o))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put min-value)
	     (send stream put max-value)
	     (send stream put init-value))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (slider-install (send stream get-exact)
			     (send stream get-exact)
			     (send stream get-exact)))])
	(super-new))))
  
  (define gb:slider-snip% (gb:make-slider-snip%
			   (gb:make-layout-snip%
			    (gb:make-callback-snip%
			     (gb:make-text-labelled-snip% gb:atomic-snip% 
							  "Slider")))
			   "gb:slider"))
  
  (register-class gb:slider-snip% "gb:slider")
  
  (define gb:make-gauge-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field vertical-layout?)
	(inherit get-label gb-need-recalc-size)
	(field
	  [max-value 10]
	  [min-height 10]
	  [min-width 50])
	(public*
	  [gauge-install
	   (lambda (mx)
	     (set! max-value mx))])
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (super-new)
	       (private-field
		 [max-val (make-number-control controls "Maximum:" 10 (lambda () 1) (lambda () 10000) 
					       (lambda (x) 
						 (set! max-value x) 
						 (gb-need-recalc-size)))])))]
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "gauge"))]
	  [init-vertical-layout? (lambda () #f)]
	  [get-min-body-size
	   (lambda (dc)
	     (if vertical-layout?
		 (values min-height min-width)
		 (values min-width min-height)))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (let ([b (send dc get-brush)])
	       (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'solid))
	       (send dc draw-rectangle 
		     x (if vertical-layout? (+ y (* 0.75 h)) y)
		     (if vertical-layout? w (* 0.25 w)) (if vertical-layout? (* 0.25 h) h))
	       (send dc set-brush b)))]
	  [gb-get-default-class (lambda () 'gauge%)]
	  [gb-instantiate-arguments
	   (lambda ()
	     (list*
	      `[range ,max-value]
	      (super gb-instantiate-arguments)))]
	  
	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o gauge-install max-value)
	       o))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put max-value))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (gauge-install (send stream get-exact)))])
	(super-new))))
  
  (define gb:gauge-snip% (gb:make-gauge-snip%
			  (gb:make-layout-snip%
			   (gb:make-text-labelled-snip% gb:atomic-snip% 
							"Gauge"))
			  "gb:gauge"))
  
  (register-class gb:gauge-snip% "gb:gauge")


  (provide gb:slider-snip%
	   gb:gauge-snip%))
