(module mrcanvas racket/base
  (require mzlib/class
           mzlib/class100
           mzlib/list
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "const.rkt"
           "kw.rkt"
           "helper.rkt"
           "check.rkt"
           "wx.rkt"
           "wxcanvas.rkt"
           "mrwindow.rkt"
           "mrcontainer.rkt"
           "mrtop.rkt")

  (provide canvas<%>
           canvas%
           editor-canvas%)

  (define canvas-default-size 20) ; a default size for canvases tht fits borders without losing client sizes
  (define canvas-scroll-size 10)
  (define canvas-control-border-extra (case (system-type)
					[(windows) 2]
					[else 0]))

  (define canvas<%>
    (interface (subwindow<%>)
      min-client-width min-client-height
      on-char on-event on-paint on-tab-in
      warp-pointer get-dc
      set-canvas-background get-canvas-background
      set-resize-corner))

  (define-keywords canvas%-keywords
    window%-keywords
    subarea%-keywords
    area%-keywords)

  (define basic-canvas%
    (class100* (make-subwindow% (make-window% #f (make-subarea% area%))) (canvas<%>) (mk-wx mismatches parent)
      (public
	[on-char (lambda (e) (send wx do-on-char e))]
	[on-event (lambda (e) (send wx do-on-event e))]
	[on-paint (lambda () (when wx (send wx do-on-paint)))]
	[on-tab-in (lambda () (void))]
	
	[min-client-width (param (lambda () wx) min-client-width)]
	[min-client-height (param (lambda () wx) min-client-height)]

	[warp-pointer (entry-point (lambda (x y) (send wx warp-pointer x y)))]

	[get-dc (entry-point (lambda () (send wx get-dc)))]
	[make-bitmap (lambda (w h)
                       (unless (exact-positive-integer? w)
                         (raise-type-error (who->name '(method canvas% make-bitmap))
                                           "exact positive integer"
                                           w))
                       (unless (exact-positive-integer? h)
                         (raise-type-error (who->name '(method canvas% make-bitmap))
                                           "exact positive integer"
                                           h))
                       (send wx make-compatible-bitmap w h))]

        [suspend-flush (lambda ()
                         (send wx begin-refresh-sequence))]
        [resume-flush (lambda ()
                        (send wx end-refresh-sequence))]
        [flush (lambda () (send wx flush))]

	[set-canvas-background
	 (entry-point
	  (lambda (c)
	    (unless (c . is-a? . wx:color%)
	      (raise-type-error (who->name '(method canvas<%> set-canvas-background))
				"color% object"
				c))
	    (unless (send wx get-canvas-background)
	      (raise-mismatch-error (who->name '(method canvas<%> set-canvas-background))
				    "cannot set a transparent canvas's background color: "
				    c))
	    (send wx set-canvas-background c)))]
	[get-canvas-background
	 (entry-point
	  (lambda ()
	    (send wx get-canvas-background)))]

	[set-resize-corner (lambda (on?)
			     (send wx set-resize-corner on?))])
      (private-field
       [wx #f])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) (lambda () wx) mismatches #f parent #f))))))

  (define default-paint-cb (lambda (canvas dc) (void)))

  (define canvas%
    (class100*/kw basic-canvas% ()
		  [(parent [style null] [paint-callback default-paint-cb] [label #f] [gl-config #f])
		   canvas%-keywords]
      (private-field [paint-cb paint-callback]
		     [has-x? (and (list? style) (memq 'hscroll style))]
		     [has-y? (and (list? style) (memq 'vscroll style))])
      (inherit get-client-size get-dc set-label 
               suspend-flush resume-flush flush
               get-canvas-background)
      (rename [super-on-paint on-paint])
      (sequence 
	(let ([cwho '(constructor canvas)])
	  (check-container-parent cwho parent)
	  (check-style cwho #f '(border hscroll vscroll gl deleted control-border combo no-autoclear 
                                        transparent resize-corner no-focus)
		       style)
	  (check-callback cwho paint-callback)
	  (check-label-string/false cwho label)))
      (public
        [on-scroll (lambda (e) (send wx do-on-scroll e))]
	[swap-gl-buffers (lambda () 
                           (let ([ctx (send (send wx get-dc) get-gl-context)])
                             (when ctx
                               (send ctx swap-buffers))))]
	[with-gl-context (lambda (thunk #:fail [fail (lambda () 
                                                       (error (who->name '(method canvas% with-gl-context))
                                                              "no gl context available"))])
			   (let ([ctx (send (send wx get-dc) get-gl-context)])
			     (if ctx
				 (send ctx call-as-current thunk)
				 (fail))))]
	[accept-tab-focus (entry-point
			   (case-lambda
			    [() (send wx get-tab-focus)]
			    [(on?) (send wx set-tab-focus (and on? #t))]))]
	[get-virtual-size (entry-point
			   (lambda () (double-boxed
				       0 0
				       (lambda (x y) (send wx get-virtual-size x y)))))]
	[get-view-start (entry-point
			 (lambda () (double-boxed
				     0 0
				     (lambda (x y) (send wx view-start x y)))))]

	[scroll (entry-point (lambda (x y) 
			       (when x (check-fraction '(method canvas% scroll) x))
			       (when y (check-fraction '(method canvas% scroll) y))
			       (send wx scroll (or x -1) (or y -1))))]

	[init-auto-scrollbars
	 (lambda (w h x y)
	   (when w (check-gauge-integer '(method canvas% init-auto-scrollbars) w))
	   (when h (check-gauge-integer '(method canvas% init-auto-scrollbars) h))
	   (check-fraction '(method canvas% init-auto-scrollbars) x)
	   (check-fraction '(method canvas% init-auto-scrollbars) y)
	   (let-values ([(cw ch) (get-client-size)])
	     (send wx set-scrollbars (if w 1 0) (if h 1 0)
		   (or w 0) (or h 0) 1 1
		   (if w (inexact->exact (floor (* x (max 0 (- w cw))))) 0)
		   (if h (inexact->exact (floor (* y (max 0 (- h ch))))) 0)
		   #t)))]
	
	[init-manual-scrollbars 
	 (lambda (x-len y-len x-page y-page x-val y-val)
	   (let ([who '(method canvas% init-auto-scrollbars)])
	     (when x-len (check-gauge-integer who x-len))
	     (when y-len (check-gauge-integer who y-len))
	     (check-gauge-integer who x-page)
	     (check-gauge-integer who y-page)
	     (check-range-integer who x-val)
	     (check-range-integer who y-val)
	     (when (and x-len (< x-len x-val))
	       (raise-mismatch-error (who->name who)
				     (format "horizontal value: ~e larger than the horizontal range: "
					     x-val)
				     x-len))
	     (when (and y-len (< y-len y-val))
	       (raise-mismatch-error (who->name who)
				     (format "vertical value: ~e larger than the vertical range: "
					     y-val)
				     y-len)))
	   (send wx set-scrollbars (if x-len 1 0) (if y-len 1 0)
		 (or x-len 0) (or y-len 0) x-page y-page x-val y-val #f))]

	[show-scrollbars
	 (lambda (x-on? y-on?)
	   (let ([bad (lambda (which what)
			(raise-mismatch-error 
			 (who->name '(method canvas% show-scrollbars))
			 (format
			  "cannot show ~a scrollbars, because the canvas style did not include ~a: "
			  which
			  what)
			 this))])
	     (when x-on? (unless has-x? (bad "horizontal" 'hscroll)))
	     (when y-on? (unless has-y? (bad "vertical" 'vscroll)))
	     (send wx show-scrollbars x-on? y-on?)))]

	[get-scroll-pos (entry-point (lambda (d) (send wx get-scroll-pos d)))]
	[set-scroll-pos (entry-point (lambda (d v) (send wx set-scroll-pos d v)))]
	[get-scroll-range (entry-point (lambda (d) (send wx get-scroll-range d)))]
	[set-scroll-range (entry-point (lambda (d v) (send wx set-scroll-range d v)))]
	[get-scroll-page (entry-point (lambda (d) (send wx get-scroll-page d)))]
	[set-scroll-page (entry-point (lambda (d v) (send wx set-scroll-page d v)))])
      (override
	[on-paint (lambda () 
		    (if (eq? paint-cb default-paint-cb)
			(super-on-paint)
			(paint-cb this (get-dc))))])
      (private-field [no-clear? (memq 'no-autoclear style)])
      (public
        [refresh-now (lambda ([do-paint (lambda (dc) (on-paint))]
                              #:flush? [flush? #t])
                       (let ([dc (get-dc)])
                         (dynamic-wind
                             (lambda ()
                               (suspend-flush))
                             (lambda ()
                               (unless no-clear?
                                 (let ([bg (get-canvas-background)])
                                   (if bg
                                       (let ([old-bg (send dc get-background)])
                                         (as-entry
                                          (lambda ()
                                            (send dc set-background bg)
                                            (send dc clear)
                                            (send dc set-background old-bg))))
                                       (send dc erase))))
                               (do-paint dc))
                             (lambda ()
                               (resume-flush)))
                         (when flush? (flush))))])
      (private-field
       [wx #f])
      (sequence
	(super-init (lambda () 
		      (let ([ds (+ (cond
				    [(memq 'control-border style) (+ 4 canvas-control-border-extra)]
				    [(memq 'border style) 4]
				    [else 0])
				   (if (or has-x? has-y?)
				       canvas-default-size
				       1))])
			(set! wx (make-object wx-canvas% this this
					      (mred->wx-container parent)
					      -1 -1 
					      (+ ds (if (memq 'combo style) side-combo-width 0)) ds
					      style
					      gl-config)))
		      wx)
		    (lambda ()
		      (let ([cwho '(constructor canvas)])
			(check-container-ready cwho parent)))
		    parent)
	(when label
	  (set-label label))
	(send parent after-new-child this))))
  
  (define editor-canvas%
    (class100*/kw basic-canvas% ()
		  [(parent [editor #f] [style null] [scrolls-per-page 100] [label #f]
			   [wheel-step no-val] [line-count no-val]
			   [horizontal-inset 5] [vertical-inset 5])
		   canvas%-keywords]
      (sequence 
	(let ([cwho '(constructor editor-canvas)])
	  (check-container-parent cwho parent)
	  (check-instance cwho internal-editor<%> "text% or pasteboard%" #t editor)
	  (check-style cwho #f '(hide-vscroll hide-hscroll no-vscroll no-hscroll auto-vscroll auto-hscroll
					      deleted control-border combo transparent no-border resize-corner
                                              no-focus)
		       style)
	  (check-gauge-integer cwho scrolls-per-page)
	  (check-label-string/false cwho label)
	  (unless (eq? wheel-step no-val)
	    (check-wheel-step cwho wheel-step))
	  (unless (or (not line-count) (eq? line-count no-val))
	    ((check-bounded-integer 1 1000 #t) cwho line-count))
	  (unless (eq? horizontal-inset 5)
	    (check-margin-integer cwho horizontal-inset))
	  (unless (eq? vertical-inset 5)
	    (check-margin-integer cwho vertical-inset))))
      (inherit set-label)
      (private-field
       [force-focus? #f]
       [scroll-to-last? #f]
       [scroll-bottom? #f])
      (public
	[call-as-primary-owner (lambda (f) (send wx call-as-primary-owner f))]
	[allow-scroll-to-last
	 (entry-point 
	  (case-lambda
	   [() scroll-to-last?]
	   [(on?) (set! scroll-to-last? (and on? #t))
	    (send wx allow-scroll-to-last on?)]))]
	[scroll-with-bottom-base
	 (entry-point
	  (case-lambda
	   [() scroll-bottom?]
	   [(on?) (set! scroll-bottom? (and on? #t))
	    (send wx scroll-with-bottom-base on?)]))]
	[lazy-refresh
	 (entry-point
	  (case-lambda
	   [() (send wx get-lazy-refresh)]
	   [(on?) (send wx set-lazy-refresh on?)]))]
	[force-display-focus
	 (entry-point
	  (case-lambda
	   [() force-focus?]
	   [(on?) (set! force-focus? (and on? #t))
	    (send wx force-display-focus on?)]))]

	[accept-tab-focus (entry-point
			   (case-lambda
			    [() (send wx get-tab-focus)]
			    [(on?) (send wx set-tab-focus (and on? #t))]))]
	[allow-tab-exit (entry-point
			 (case-lambda
			  [() (send wx is-tabable?)]
			  [(on?) (send wx set-tabable (and on? #t))]))]

	[set-line-count
	 (entry-point
	  (lambda (n)
	    ((check-bounded-integer 1 1000 #t) '(method editor-canvas% set-line-count) n)
	    (send wx set-line-count n)))]
	[get-line-count
	 (entry-point
	  (lambda ()
	    (send wx get-line-count)))]

	[scroll-to (case-lambda 
		    [(x y w h refresh?) (send wx scroll-to x y w h refresh?)]
		    [(x y w h refresh? bias) (send wx scroll-to x y w h refresh? bias)])]

	[get-editor (entry-point (lambda () (send wx get-editor)))]
	[set-editor (entry-point 
		     (case-lambda 
		      [(m) (send wx set-editor m)]
		      [(m upd?) (send wx set-editor m upd?)]))]
	[(ws wheel-step)
	 (case-lambda 
	  [() (let ([v (send wx get-wheel-step)])
		(if (zero? v) #f v))]
	  [(wheel-step)
	   (check-wheel-step '(method editor-canvas% wheel-step) wheel-step)
	   (send wx set-wheel-step (or wheel-step 0))])]
	[(vi vertical-inset)
	 (entry-point
	  (case-lambda
	   [() (send wx get-y-margin)]
	   [(m) 
	    (check-margin-integer '(method editor-canvas% vertical-inset) m)
	    (as-exit (lambda () (send wx set-y-margin m)))]))]
	[(hi horizontal-inset)
	 (entry-point
	  (case-lambda
	   [() (send wx get-x-margin)]
	   [(m) 
	    (check-margin-integer '(method editor-canvas% horizontal-inset) m)
	    (as-exit (lambda () (send wx set-x-margin m)))]))])
      (private-field
       [wx #f])
      (sequence
	(super-init (lambda () 
		      (let* ([no-h? (or (memq 'no-vscroll style)
					(memq 'hide-vscroll style))]
			     [no-v? (or (memq 'no-hscroll style)
					(memq 'hide-hscroll style))]
			     [get-ds (lambda (no-this? no-other?)
				       (+ (if (memq 'control-border style)
					      canvas-control-border-extra
					      0)
					  (cond
					   [(and no-this? no-other?) 14]
					   [no-this? canvas-default-size]
					   [else (+ canvas-scroll-size canvas-default-size)])))])
			(set! wx (make-object wx-editor-canvas% this this
					      (mred->wx-container parent) -1 -1
					      (+ (get-ds no-h? no-v?) (if (memq 'combo style) side-combo-width 0))
					      (get-ds no-v? no-h?)
					      #f 
					      (append
					       (if (memq 'no-border style)
						   null
						   '(border))
					       (remq 'no-border style))
					      scrolls-per-page #f))
			wx))
		    (lambda () 
		      (let ([cwho '(constructor editor-canvas)])
			(check-container-ready cwho parent)))
		    parent)
	(unless (eq? wheel-step no-val)
	  (ws wheel-step))
	(when label
	  (set-label label))
	(when editor
	  (set-editor editor))
	(send parent after-new-child this)
	(unless (or (not line-count) (eq? line-count no-val))
	  (set-line-count line-count))
	(unless (or (eq? vertical-inset 5))
	  (vi vertical-inset))
	(unless (or (eq? horizontal-inset 5))
	  (hi horizontal-inset))))))
