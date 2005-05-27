
(module slides-to-picts mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "unitsig.ss")
	   (lib "etc.ss")
	   "sig.ss"
	   "param.ss"
	   "core.ss"
	   (lib "mrpict.ss" "texpict"))

  (provide get-slides-as-picts)

  (define get-slides-as-picts
    (opt-lambda (file w h c? [stop-after #f])
      (let ([ns (make-namespace-with-mred)]
	    [orig-ns (current-namespace)]
	    [param ((current-module-name-resolver) '(lib "param.ss" "slideshow") #f #f)]
	    [core ((current-module-name-resolver) '(lib "core.ss" "slideshow") #f #f)]
	    [slides null]
	    [xs (/ w 1024)]
	    [ys (/ h 768)]
	    [escape void])
	(parameterize ([current-namespace ns])
	  (namespace-attach-module orig-ns param)
	  (namespace-attach-module orig-ns core))
	(current-slideshow-linker
	 (lambda (core@)
	   (compound-unit/sig
	    (import)
	    (link [CONFIG : config^ ((unit/sig config^
				       (import)
				       (define base-font-size 32)
				       (define screen-w 1024)
				       (define screen-h 768)
				       (define use-screen-w w)
				       (define use-screen-h h)
				       (define pixel-scale 1)
				       (define condense? c?)
				       (define printing? #f)
                                       (define smoothing? #t)))]
		  [CORE : core^ (core@ CONFIG (VIEWER : viewer^))]
		  [VIEWER : viewer^ ((unit/sig viewer^
				       (import)
				       (define (add-talk-slide! s)
					 (set! slides (cons s slides))
					 (when (and stop-after
						    ((length slides) . >= . stop-after))
					   (escape (void))))
				       (define (retract-talk-slide!)
					 (set! slides (cdr slides)))
				       (define (most-recent-talk-slide)
					 (and (pair? slides) (car slides)))
				       (define display-progress void)
				       (define set-init-page! void)
				       (define set-use-background-frame! void)
				       (define enable-click-advance! void)
				       (define set-page-numbers-visible! void)
				       (define add-click-region! void)
				       (define done-making-slides void)))])
	    (export (open CORE) (unit CONFIG config) (unit VIEWER viewer)))))
	(parameterize ([current-namespace ns])
	  (let/ec k
	    (set! escape k)
	    (dynamic-require `(file ,file) #f)))
	(map (lambda (s)
	       (let ([drawer (sliderec-drawer s)])
		 (dc (lambda (dc x y)
		       (let-values ([(orig-xs orig-ys) (send dc get-scale)])
			 (send dc set-scale (* orig-xs xs) (* orig-ys ys))
			 (drawer dc (+ (/ x xs) 20) (+ (/ y ys) 20))
			 (send dc set-scale orig-xs orig-ys)))
		     w h 0 0)))
	     (reverse slides))))))
