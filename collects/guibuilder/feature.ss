
(module feature mzscheme
  (require (prefix mred: mred)
	   mzlib/class
	   mzlib/file
	   mzlib/pretty
	   mzlib/etc
	   mzlib/list
	   "base.ss"
	   "utils.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mixins for GUI features
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define gb:make-text-label-snip%
    (lambda (cl deflabel)
      (class cl
	(inherit get-style gb-need-recalc-size)
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (override*
		 [get-kind (lambda () deflabel)])
	       (super-new)
	       (private-field
		 [label-buffer (make-one-line/callback-edit controls "Label:"
							    (lambda (txt)
							      (set! label txt)
							      (gb-need-recalc-size))
							    label)])))]
	  [gb-instantiate-arguments
	   (lambda ()
	     `(,@(super gb-instantiate-arguments)
	       [label ,(get-label)]))])
	(field
	  [label deflabel])
	(public*
	  [get-label
	   (lambda ()
	     label)]
	  [get-label-size
	   (lambda (dc)
	     (let-values ([(w h d a) (send dc get-text-extent label 
					   (send (get-style) get-font))])
	       (values w h)))]
	  [draw-label
	   (lambda (dc x y)
	     (send dc draw-text label x y))]
	  
	  [label-install
	   (lambda (n)
	     (set! label n))])
	(override*
	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o label-install label)
	       o))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put (string->bytes/utf-8 label)))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (label-install ((get-bytes->string version) (send stream get-bytes))))])
	(super-new))))

  (define gb:make-callback-snip%
    (lambda (cl)
      (class cl
	(inherit-field name)
	(public*
	  [get-callback-kinds (lambda () (list "-callback"))]
	  [get-callback-code (lambda ()
			       (map (lambda (x) '(lambda (w e) (void))) (get-callback-kinds)))]
	  [get-callback-names
	   (lambda ()
	     (map
	      (lambda (ct)
		(string->symbol (string-append name ct)))
	      (get-callback-kinds)))]
	  [gb-get-unified-callback
	   (lambda ()
	     `(lambda (b e) (,(car (get-callback-names)) b e)))])
	(override*
	  [gb-instantiate-arguments
	   (lambda ()
	     `(,@(super gb-instantiate-arguments)
	       [callback ,(gb-get-unified-callback)]))]
	  [gb-aux-instantiate
	   (lambda (mode)
	     (append
	      (if (or (output-mode-as-class? mode)
		      (output-mode-no-free-vars? mode))
		  (map (lambda (n c) 
			 (if (output-mode-as-class? mode)
			     `(public* [,n ,c])
			     `(define ,n ,c)))
		       (get-callback-names) (get-callback-code))
		  null)
	      (super gb-aux-instantiate mode)))])
	(super-new))))
  
  (define gb:make-text-labelled-snip%
    (lambda (cl deflabel)
      (class (gb:make-text-label-snip% cl deflabel)
	(inherit-field w h)
	(inherit get-label-size draw-label gb-need-recalc-size)
	(private-field
	  [hmargin 2]
	  [vertical-label? (init-vertical-label?)])
	(public*
	  [get-label-top-margin (lambda () 0)]
	  [init-vertical-label? (lambda () #f)]

	  [get-min-body-size
	   (lambda (dc)
	     (values 0 0))]
	  [draw-body
	   (lambda (dc x y w h)
	     (void))]
	  
	  [labelpos-install
	   (lambda (vert?)
	     (set! vertical-label? vert?))])
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (super-new)
	       (private-field
		 [direction-radio
		  (make-object mred:radio-box% "Label Position:" '("Top" "Left")
			       controls 
			       (lambda (r e)
				 (set! vertical-label? (zero? (send direction-radio get-selection)))
				 (gb-need-recalc-size))
			       '(horizontal))])
	       (send direction-radio set-selection (if vertical-label? 0 1))))]
	  [gb-get-min-size
	   (lambda (dc)
	     (let-values ([(x y) (get-label-size dc)]
			  [(x2 y2) (get-min-body-size dc)]
			  [(+x +y) (if vertical-label?
				       (values max +)
				       (values (lambda (a b) (+ a b hmargin)) max))])
	       (values (+x x x2) (+y (+ y (get-label-top-margin)) y2))))]
	  [draw
	   (lambda (dc x y . other)
	     (draw-label dc x (+ y (get-label-top-margin)))
	     (let*-values ([(lw lh) (get-label-size dc)]
			   [(dx dy) (if vertical-label?
					(values 0 lh)
					(values (+ lw hmargin) 0))])
	       (with-clipping-region dc (+ x dx) (+ y dy) (- w dx) (- h dy)
		 (lambda ()
		   (draw-body dc (+ x dx) (+ y dy) (- w dx) (- h dy))))))]
	  
	  [gb-get-style
	   (lambda ()
	     (cons (if vertical-label?
		       'vertical-label
		       'horizontal-label)
		   (super gb-get-style)))]

	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o labelpos-install vertical-label?)
	       o))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put (if vertical-label? 1 0)))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (labelpos-install (positive? (send stream get-exact))))])
	(super-new))))


  
  (define gb:make-layout-snip%
    (lambda (cl)
      (class cl
	(inherit gb-need-recalc-size)
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (super-new)
	       (private-field
		 [layout-direction-radio
		  (make-object mred:radio-box%
			       "Layout:"
			       '("Vertical" "Horizontal")
			       controls 
			       (lambda (r e)
				 (set! vertical-layout? 
				       (zero? (send layout-direction-radio get-selection)))
				 (gb-need-recalc-size))
			       '(horizontal))])
	       (send layout-direction-radio set-selection (if vertical-layout? 0 1))))])
	(field
	  [vertical-layout? (init-vertical-layout?)])
	(public*
	  [init-vertical-layout? (lambda () #t)]
	  [layout-install
	   (lambda (vert?)
	     (set! vertical-layout? vert?))])
	(override*
	 [gb-get-style
	  (lambda ()
	    (cons (if vertical-layout?
		      'vertical
		      'horizontal)
		  (super gb-get-style)))]
	 [copy
	  (lambda ()
	    (let ([o (super copy)])
	      (send o layout-install vertical-layout?)
	      o))]
	 [write
	  (lambda (stream)
	    (super write stream)
	    (send stream put (if vertical-layout? 1 0)))]
	 [read
	  (lambda (stream version)
	    (super read stream version)
	    (layout-install (positive? (send stream get-exact))))])
	(super-new))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mixins for configuration options
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define gb:make-configure-snip%
    (lambda (cl tag init)
      (class cl
	(private-field
	  [v init])
	(override*
	  [get-tagged-value
	   (lambda (t)
	     (if (eq? t tag)
		 v
		 (super get-tagged-value t)))]
	  [set-tagged-value
	   (lambda (t v-in)
	     (if (eq? t tag)
		 (set! v v-in)
		 (super set-tagged-value t v-in)))]

	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o set-tagged-value tag v)
	       o))])
	(super-new))))

  (define gb:make-boolean-configure-snip%
    (lambda (cl tag label init change-cb init-cb)
      (class (gb:make-configure-snip% cl tag init)
	(inherit gb-need-recalc-size get-tagged-value set-tagged-value)
	(override*
	  [get-frame%
	   (lambda ()
	     (define this-snip this)
	     (class (super get-frame%)
	       (inherit-field controls)
	       (override*
		[find-control
		 (lambda (t)
		   (if (eq? t tag)
		       c
		       (super find-control t)))])
	       (super-new)
	       (private-field
		 [c (make-object mred:check-box% 
				 label controls
				 (lambda (c e)
				   (set-tagged-value tag (send c get-value))
				   (change-cb this this-snip)
				   (gb-need-recalc-size)))])
	       (send c set-value (get-tagged-value tag))
	       (init-cb this this-snip)))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put (if (get-tagged-value tag) 1 0)))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (set-tagged-value tag (positive? (send stream get-exact))))])
	(super-new))))

  (define gb:make-multi-checkable-snip%
    (lambda (cl)
      (class (gb:make-boolean-configure-snip% cl 'multi "Multiple Lines" #f
						  (lambda (f snip)
						    (send snip multi-changed f))
						  void)
	 (inherit get-tagged-value)
	 (public*
	  [get-multi
	   (lambda () (get-tagged-value 'multi))]
	  [multi-changed
	   (lambda (f)
	     (send (send f find-control 'hscroll) enable (get-multi)))])
	 (super-new))))
  
  (define gb:make-select-configure-snip%
    (lambda (cl tag label choices)
      (class (gb:make-configure-snip% cl tag 0)
	(inherit gb-need-recalc-size get-tagged-value set-tagged-value)
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (super-new)
	       (private-field
		 [c (make-object mred:choice% 
				 label choices controls
				 (lambda (c e)
				   (set-tagged-value tag (send c get-selection))
				   (gb-need-recalc-size)))])
	       (send c set-selection (get-tagged-value tag))))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put (get-tagged-value tag)))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (set-tagged-value tag (send stream get-exact)))])
	(super-new))))
  
  (provide gb:make-text-label-snip%
	   gb:make-callback-snip%
	   gb:make-text-labelled-snip%
	   gb:make-layout-snip%

	   gb:make-configure-snip%
	   gb:make-boolean-configure-snip%
	   gb:make-multi-checkable-snip%
	   gb:make-select-configure-snip%))
