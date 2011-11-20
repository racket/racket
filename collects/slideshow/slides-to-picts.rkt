(module slides-to-picts scheme/base
  (require racket/draw
           scheme/class
           scheme/unit
           "sig.rkt"
           "param.rkt"
           "core.rkt"
           texpict/mrpict)

  (provide get-slides-as-picts)

  (define-namespace-anchor anchor)

  (define get-slides-as-picts
    (lambda (file w h c? [stop-after #f])
      (let ([ns (make-base-namespace)]
	    [orig-ns (namespace-anchor->empty-namespace anchor)]
	    [slides null]
	    [xs (/ w 1024)]
	    [ys (/ h 768)]
	    [escape void])
	(parameterize ([current-namespace ns])
	  (namespace-attach-module orig-ns 'slideshow/param)
	  (namespace-attach-module orig-ns 'slideshow/core))
	(current-slideshow-linker
	 (lambda (core@)
	   (compound-unit
	    (import)
	    (export CORE CONFIG VIEWER)
	    (link [((CONFIG : config^)) (unit 
                                          (import)
                                          (export config^)
                                          (define base-font-size 32)
                                          (define screen-w 1024)
                                          (define screen-h 768)
                                          (define use-screen-w w)
                                          (define use-screen-h h)
                                          (define pixel-scale 1)
                                          (define condense? c?)
                                          (define printing? #f)
                                          (define smoothing? #t)
                                          (define commentary-on-slide? #f))]
		  [((CORE : core^)) core@ CONFIG VIEWER]
		  [((VIEWER : viewer^)) (unit
                                          (import (prefix c: core^))
                                          (export viewer^)
                                          (define (add-talk-slide! s)
                                            (set! slides (cons (list s (c:get-margin)) slides))
                                            (when (and stop-after
                                                       ((length slides) . >= . stop-after))
                                              (escape (void))))
                                          (define (retract-talk-slide!)
                                            (set! slides (cdr slides)))
                                          (define (most-recent-talk-slide)
                                            (and (pair? slides) (caar slides)))
                                          (define display-progress void)
                                          (define set-init-page! void)
                                          (define set-use-background-frame! void)
                                          (define enable-click-advance! void)
                                          (define set-page-numbers-visible! void)
                                          (define add-click-region! void)
                                          (define done-making-slides void))
                   CORE]))))
	(parameterize ([current-namespace ns])
	  (let/ec k
	    (set! escape k)
	    (dynamic-require `(file ,file) #f)))
	(map (lambda (s)
	       (let ([drawer (sliderec-drawer (car s))]
                     [margin (cadr s)])
		 (dc (lambda (dc x y)
		       (let-values ([(orig-xs orig-ys) (send dc get-scale)])
			 (send dc set-scale (* orig-xs xs) (* orig-ys ys))
			 (drawer dc (+ (/ x xs) margin) (+ (/ y ys) margin))
			 (send dc set-scale orig-xs orig-ys)))
		     w h 0 0)))
	     (reverse slides))))))
