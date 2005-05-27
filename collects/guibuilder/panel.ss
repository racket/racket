
(module panel mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss"
	   "base.ss")

  (define gb:make-panel-params-snip%
    (lambda (cl)
      (class cl
	(inherit-field horizontal-child-alignment
		       vertical-child-alignment
		       with-border?)
	(inherit set-horizontal-child-alignment
		 set-vertical-child-alignment
		 set-with-border
		 gb-need-recalc-size)
	(override*
	 [get-frame%
	  (lambda ()
	    (class (super get-frame%)
	      (inherit-field controls)
	      (super-new)
	      (field
		[hca-choice
		 (make-object mred:choice%
			      "Horizontal Align Children:"
			      '("Left" "Center" "Right")
			      controls 
			      (lambda (r e)
				(set-horizontal-child-alignment
				 (add1 (send r get-selection)))
				(gb-need-recalc-size)))]
		[vca-choice
		 (make-object mred:choice% 
			      "Vertical Align Children:"
			      '("Top" "Center" "Bottom")
			      controls 
			      (lambda (r e)
				(set-vertical-child-alignment
				 (add1 (send r get-selection)))
				(gb-need-recalc-size)))]
		[border-check
		 (make-object mred:check-box%
			      "Show Border" controls
			      (lambda (c e)
				(set-with-border (send c get-value))
				(gb-need-recalc-size)))])
	      (send hca-choice stretchable-width #f)
	      (send hca-choice set-selection (sub1 horizontal-child-alignment))
	      (send vca-choice stretchable-width #f)
	      (send vca-choice set-selection (sub1 vertical-child-alignment))
	      (send border-check set-value with-border?)))])
	(private*
	  [symbol-append
	   (lambda (a b) (string->symbol (string-append (symbol->string a) (symbol->string b))))])
	(override*
	 [gb-instantiate-arguments
	  (lambda ()
	    `(,@(super gb-instantiate-arguments)
	      [alignment '(,(case horizontal-child-alignment
			      [(1) 'left]
			      [(2) 'center]
			      [(3) 'right])
			   ,(case horizontal-child-alignment
			      [(1) 'top]
			      [(2) 'center]
			      [(3) 'bottom]))]))])
	(super-new))))

  (define gb:vertical-panel-snip% 
    (class (gb:make-panel-params-snip% gb:snip%)
      (override*
	[get-classname (lambda () "gb:vertical-panel")]
	[init-name (lambda () (new-name "vpanel"))])
      (super-new)))
  
  (register-class gb:vertical-panel-snip% "gb:vertical-panel")

  ; Used by top-level panel:
  (define gb:panel-snip%
    (class gb:vertical-panel-snip%
      (override*
	[get-classname (lambda () "gb:panel")])
      (super-new)))
  
  (register-class gb:panel-snip% "gb:panel")
  
  (define gb:horizontal-panel-snip%
    (class (gb:make-panel-params-snip% gb:snip%)
      (inherit-field horizontal-child-alignment vertical-child-alignment
		     children)
      (inherit spacing-+)
      (override*
	[get-classname (lambda () "gb:horizontal-panel")]
	[init-name (lambda () (new-name "hpanel"))]
	
	(gb-get-child-x-start
	 (lambda (mw mh w h)
	   (if (or (= horizontal-child-alignment 1)
		   (ormap (lambda (c) (gb-x-stretch? c)) children))
	       0
	       (case horizontal-child-alignment
		 [(2) (/ (- w mw) 2)]
		 [(3) (- w mw)]))))
	(gb-get-child-y-start
	 (lambda (mw mh w h) 
	   0))

	(gb-combine-child-width (lambda (a b) (spacing-+ a b)))
	(gb-combine-child-height (lambda (a b) (max a b)))
	
	(gb-compute-child-x-pos
	 (lambda (dc c w)
	   0))
	(gb-compute-child-y-pos
	 (lambda (dc c h)
	   (if (gb-y-stretch? c)
	       0
	       (case vertical-child-alignment
		 [(2) (let-values ([(cw ch) (send c gb-get-min-size dc)])
			 (/ (- h ch) 2))]
		 [(1) 0]
		 [(3) (let-values ([(cw ch) (send c gb-get-min-size dc)])
			 (- h ch))]))))
	(gb-compute-child-width
	 (lambda (dc c w xsc dw)
	   (let-values ([(cw ch) (send c gb-get-min-size dc)])
	     (if (gb-x-stretch? c)
		 (+ cw (/ dw xsc))
		 cw))))
	(gb-compute-child-height
	 (lambda (dc c h ysc dh)
	   (if (gb-y-stretch? c)
	       h
	       (let-values ([(cw ch) (send c gb-get-min-size dc)])
		 ch))))
	
	(gb-combine-child-x-offset (lambda (a b) (spacing-+ a b)))
	(gb-combine-child-y-offset (lambda (a b) a))
	
	(find-position-<
	 (lambda (fx fy cx cy)
	   (< fx cx)))
	
	[gb-get-default-class (lambda () 'horizontal-panel%)])
      (super-new)))

  (register-class gb:horizontal-panel-snip% "gb:horizontal-panel")

  (provide gb:make-panel-params-snip%
	   gb:vertical-panel-snip% 
	   gb:panel-snip%
	   gb:horizontal-panel-snip%))
