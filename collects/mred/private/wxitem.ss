(module wxitem mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "etc.ss")
	   (lib "file.ss")
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "helper.ss"
	   "const.ss"
	   "wx.ss"
	   "check.ss"
	   "wxwindow.ss")

  (provide (protect make-item%
		    make-control%
		    make-simple-control%
		    wx-button%
		    wx-check-box%
		    wx-choice%
		    wx-message%
		    wx-gauge%
		    wx-list-box%
		    wx-radio-box%
		    wx-slider%))

  ;; make-item%: creates items which are suitable for placing into
  ;;  containers.
  ;; input: item%: a wx:item% descendant (but see below) from which the
  ;;          new class will be derived.
  ;;        stretch-x/stretch-y: booleans which specify the default
  ;;          stretchability behavior for the new class.
  ;; returns: a class, descended from wx:item%, which is suitable for
  ;;            placing in a container.
  ;; Note: the item% parameter does not necessarily HAVE to be a
  ;; descendant of wx:item%, so long as it contains the identifiers in the
  ;; inherit section below.  You will note below that I ran wx:panel%
  ;; through this function to create panel%.

  (define make-item%
    (lambda (item% x-margin-w y-margin-h stretch-x stretch-y)
      (class100 (wx-make-window% item% #f) (window-style . args)
	(inherit get-width get-height get-x get-y
		 get-parent get-client-size)
	(private-field [enabled? #t])
	(override
	  [enable
	   (lambda (b)
	     (set! enabled? (and b #t))
	     (super enable b))]

	  ;; set-size: caches calls to set-size to avoid unnecessary work,
	  ;;           and works with windowsless panels
	  ;; input: x/y: new position for object
	  ;;        width/height: new size for object
	  ;; returns: nothing
	  ;; effect: if arguments mark a different geometry than the object's
	  ;;   current geometry, passes args to super-class's set-size.
	  ;;   Otherwise, does nothing.
	  [set-size
	   (lambda (x y width height)
	     (set! x (+ x (send (area-parent) dx)))
	     (set! y (+ y (send (area-parent) dy)))
	     (unless (and (same-dimension? x (get-x))
			  (same-dimension? y (get-y))
			  (same-dimension? width (get-width))
			  (same-dimension? height (get-height)))
	       (super set-size x y width height)))])

	(public
	  [is-enabled?
	   (lambda () enabled?)])

	(private-field
	 ;; Store minimum size of item.  
	 ;; This will never change after the item is created.
	 hard-min-width
	 hard-min-height)
	(public
	  [set-min-height (lambda (v) (set! hard-min-height v) (min-height v))]
	  [set-min-width (lambda (v) (set! hard-min-width v) (min-width v))]
	  [get-hard-minimum-size (lambda () (values hard-min-width hard-min-height))]
	  
	  [client-inset
	   (lambda (h?)
	     (let ([h #f][w #f])
	       (unless h
		 (let ([w-box (box 0)]
		       [h-box (box 0)])
		   (get-client-size w-box h-box)
		   (set! h (- (get-height) (unbox h-box)))
		   (set! w (- (get-width) (unbox w-box)))))
	       (if h? h w)))]

	  ;; gets/sets user's requirement for minimum width.  Errors out
	  ;; if new value is not a non-negative real number.  Forces a
	  ;; redraw upon a set.
	  [min-client-width
	   (case-lambda 
	    [() (- (min-width) (client-inset #f))]
	    [(new-width)
	     (check-range-integer '(method canvas<%> min-client-width) new-width)
	     (min-width (+ new-width (client-inset #f)))])]
	  [min-client-height
	   (case-lambda 
	    [() (- (min-height) (client-inset #t))]
	    [(new-height) 
	     (check-range-integer '(method canvas<%> min-client-height) new-height)
	     (min-height (+ new-height (client-inset #t)))])])

	(private-field [-mw 0]
		       [-mh 0]
		       [-xm x-margin-w]
		       [-ym y-margin-h]
		       [-sx stretch-x]
		       [-sy stretch-y]
		       [first-arg (car args)])

	(public
	  [min-width
	   (mk-param
	    -mw identity
	    (lambda (v)
	      (check-range-integer '(method area<%> min-width) v))
	    force-redraw)]
	  [min-height
	   (mk-param
	    -mh identity
	    (lambda (v)
	      (check-range-integer '(method area<%> min-height) v))
	    force-redraw)]
	  
	  [x-margin
	   (mk-param
	    -xm identity
	    (lambda (v)
	      (check-margin-integer '(method subarea<%> horiz-margin) v)
	      v)
	    force-redraw)]
	  [y-margin
	   (mk-param
	    -ym identity
	    (lambda (v) 
	      (check-margin-integer '(method subarea<%> vert-margin) v)
	      v)
	    force-redraw)]

	  [stretchable-in-x
	   (mk-param -sx (lambda (x) (and x #t)) void force-redraw)]
	  [stretchable-in-y
	   (mk-param -sy (lambda (x) (and x #t)) void force-redraw)]
	  
	  ;; get-info: passes necessary info up to parent.
	  ;; input: none
	  ;; returns: child-info struct containing the info about this
	  ;;   item.
	  ;; intended to be called by item's parent upon resize.
	  [get-info
	   (lambda ()
	     (let* ([min-size (get-min-size)]
		    [result (make-child-info (car min-size) (cadr min-size)
					     (x-margin) (y-margin)
					     (stretchable-in-x)
					     (stretchable-in-y))])
	       result))]
	  
	  [area-parent (lambda () first-arg)]

	  ;; force-redraw: unconditionally trigger redraw.
	  ;; input: none
	  ;; returns: nothing
	  ;; effects: forces the item's parent (if it exists) to redraw
	  ;;   itself. This will recompute the min-size cache if it is
	  ;;   invalid.
	  [force-redraw
	   (lambda ()
	     (let ([parent (area-parent)])
	       (when parent
		 (send parent child-redraw-request this))))]
	  
	  [on-container-resize (lambda () (void))] ; This object doesn't contain anything

	  [init-min (lambda (x) x)]
	  
	  ;; get-min-size: computes the minimum size the item can
	  ;;   reasonably assume.
	  ;; input: none
	  ;; returns: a list containing the minimum width & height.
	  [get-min-size
	   (lambda ()
	     (let ([w (+ (* 2 (x-margin)) (max hard-min-width (min-width)))]
		   [h (+ (* 2 (y-margin)) (max hard-min-height (min-height)))])
	       (list w h)))])
	
	(sequence
	  (apply super-init (send (car args) get-window) (cdr args))
	  (set-min-width (init-min (get-width)))
	  (set-min-height (init-min (get-height)))

	  (unless (memq 'deleted window-style)
	    ;; For a pane[l], the creator must call the equivalent of the following,
	    ;;  delaying to let the panel's wx field get initialized before
	    ;;  panel-sizing methods are called
	    (unless (is-a? this wx-basic-panel<%>)
	      (send (area-parent) add-child this)))))))

  ;; make-control% - for non-panel items
  (define (make-control% item% x-margin y-margin
			 stretch-x stretch-y)
    (class100 (make-item% item% x-margin y-margin stretch-x stretch-y) args
      (inherit get-parent)
      (sequence
	(apply super-init args)
	(send (get-parent) set-item-cursor 0 0))))

  (define (make-simple-control% item%)
    (make-control% item%
		   const-default-x-margin const-default-y-margin 
		   #f #f))

  (define wx-button% (make-window-glue% 
		      (class100 (make-simple-control% wx:button%) (parent cb label x y w h style font)
			(inherit command set-border get-top-level)
			(private-field 
			 [border? (memq 'border style)]
			 [border-on? border?])
			(public 
			  [defaulting (lambda (on?)
					(set! border-on? on?)
					(set-border border-on?))] 
			  [has-border? (lambda () border-on?)])
			(override
			  [char-to (lambda ()
				     (as-exit
				      (lambda ()
					(command (make-object wx:control-event% 'button)))))])
			(sequence (super-init style parent cb label x y w h style font)
				  (when border?
				    (send (get-top-level) add-border-button this))))))
  (define wx-check-box% (class100 (make-window-glue% (make-simple-control% wx:check-box%)) (mred proxy parent cb label x y w h style font)
			  (inherit set-value get-value command)
			  (override
			    [char-to (lambda ()
				       (as-exit
					(lambda ()
					  (set-value (not (get-value)))
					  (command (make-object wx:control-event% 'check-box)))))])
			  (sequence (super-init mred proxy style parent cb label x y w h style font))))
  (define wx-choice% (class100 (make-window-glue% (make-simple-control% wx:choice%)) (mred proxy parent cb label x y w h choices style font)
		       (override 
			 [handles-key-code 
			  (lambda (x alpha? meta?) 
			    (or (memq x '(up down))
				(and alpha? (not meta?))))])
		       (sequence (super-init mred proxy style parent cb label x y w h choices style font))))
  (define wx-message% (class100 (make-window-glue% (make-simple-control% wx:message%)) (mred proxy parent label x y style font)
			(override [gets-focus? (lambda () #f)])
			(sequence (super-init mred proxy style parent label x y style font))))

  (define wx-gauge%
    (make-window-glue% 
     (class100 (make-control% wx:gauge% 
			      const-default-x-margin const-default-y-margin 
			      #f #f)
	 (parent label range style font)
       (inherit get-client-size get-width get-height set-size 
		stretchable-in-x stretchable-in-y set-min-height set-min-width
		get-parent)
       (override [gets-focus? (lambda () #f)])
       (private-field
	;; # pixels per unit of value.
	[pixels-per-value 1])
       (sequence
	 (super-init style parent label range -1 -1 -1 -1 style font)

	 (let-values ([(client-width client-height) (get-two-int-values 
						     (lambda (a b) (get-client-size a b)))])
	   (let ([delta-w (- (get-width) client-width)]
		 [delta-h (- (get-height) client-height)]
		 [vertical-labels? (eq? (send (send (get-parent) get-window) get-label-position) 'vertical)]
		 [horizontal? (memq 'horizontal style)])
	     (set-min-width (if horizontal?
				(let ([cw (min const-max-gauge-length
					       (* range pixels-per-value))])
				  (max (if vertical-labels?
					   cw
					   (+ cw delta-w))
				       (get-width)))
				;; client-height is the default
				;; dimension in the minor direction.
				(+ client-width delta-w)))
	     (set-min-height (if horizontal?
				 (+ client-height delta-h)
				 (let ([ch (min const-max-gauge-length
						(* range pixels-per-value))])
				   (max (if vertical-labels?
					    (+ ch delta-h)
					    ch)
					(get-height)))))))

	 (if (memq 'horizontal style)
	     (begin
	       (stretchable-in-x #t)
	       (stretchable-in-y #f))
	     (begin
	       (stretchable-in-x #f)
	       (stretchable-in-y #t)))))))

  (define list-box-wheel-step #f)

  (define wx-list-box%
    (make-window-glue% 
     (class100 (make-control% wx:list-box%
			      const-default-x-margin const-default-y-margin 
			      #t #t) (parent cb label kind x y w h choices style font label-font)
       (inherit get-first-item
		set-first-visible-item)
       (private
	 [scroll (lambda (dir)
		   (unless list-box-wheel-step
		     (set! list-box-wheel-step (get-preference '|MrEd:wheelStep| (lambda () 3)))
		     (unless (and (number? list-box-wheel-step)
				  (exact? list-box-wheel-step)
				  (integer? list-box-wheel-step)
				  (<= 1 list-box-wheel-step 100))
		       (set! list-box-wheel-step 3)))
		   (let ([top (get-first-item)])
		     (set-first-visible-item
                      (max 0 (+ top (* list-box-wheel-step dir))))))])
       (override
	 [handles-key-code (lambda (x alpha? meta?)
			     (case x
			       [(up down) #t]
			       [else (and alpha? (not meta?))]))]
	 [pre-on-char (lambda (w e)
			(or (super pre-on-char w e)
			    (case (send e get-key-code)
			      [(wheel-up) (scroll -1) #t]
			      [(wheel-down) (scroll 1) #t]
			      [else #f])))])
       (sequence (super-init style parent cb label kind x y w h choices style font label-font)))))

  (define wx-radio-box%
    (make-window-glue% 
     (class100 (make-simple-control% wx:radio-box%) (parent cb label x y w h choices major style font)
       (inherit number orig-enable set-selection command)
       (override
	 [enable
	  (case-lambda
	   [(on?) (super enable on?)]
	   [(which on?) (when (< -1 which (number))
			  (vector-set! enable-vector which (and on? #t))
			  (orig-enable which on?))])]
	 [is-enabled?
	  (case-lambda
	   [() (super is-enabled?)]
	   [(which) (and (< -1 which (number))
			 (vector-ref enable-vector which))])])

       (private-field [is-vertical? (memq 'vertical style)])
       (public
	 [vertical? (lambda () is-vertical?)]
	 [char-to-button (lambda (i)
			   (as-exit
			    (lambda ()
			      (set-selection i)
			      (command (make-object wx:control-event% 'radio-box)))))])

       (sequence (super-init style parent cb label x y w h choices major style font))

       (private-field [enable-vector (make-vector (number) #t)]))))

  (define wx-slider%
    (make-window-glue% 
     (class100 (make-control% wx:slider% 
			      const-default-x-margin const-default-y-margin 
			      #f #f)
	 (parent func label value min-val max-val style font)
       (inherit set-min-width set-min-height stretchable-in-x stretchable-in-y
		get-client-size get-width get-height get-parent)
       (private-field
	;; # pixels per possible setting.
	[pixels-per-value 3])
       ;; 3 is good because with horizontal sliders under Xt, with 1 or 2
       ;; pixels per value, the thumb is too small to display the number,
       ;; which looks bad.
       
       (sequence
	 (super-init style parent func label value min-val max-val -1 -1 -1 style font)
	 
	 (let-values ([(client-w client-h) (get-two-int-values (lambda (a b)
								 (get-client-size a b)))])
	   (let* ([horizontal? (memq 'horizontal style)]
		  [vertical-labels? (eq? (send (send (get-parent) get-window) get-label-position) 'vertical)]
		  [range (+ (* pixels-per-value (add1 (- max-val min-val)))
			    (cond
			     [(and horizontal? (not vertical-labels?)) (- (get-width) client-w)]
			     [(and (not horizontal?) vertical-labels?) (- (get-height) client-h)]
			     [else 0]))])
	     ((if horizontal? (lambda (v) (set-min-width v)) (lambda (v) (set-min-height v)))
	      (max ((if horizontal? (lambda () (get-width)) (lambda () (get-height))))
		   (min const-max-gauge-length range)))
	     (stretchable-in-x horizontal?)
	     (stretchable-in-y (not horizontal?))))))))

  )

