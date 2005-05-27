
(module canvas mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss"
	   "base.ss"
	   "feature.ss")

  (define gb:make-canvas-hscroll-checkable-snip%
    (lambda (cl)
      (class (gb:make-boolean-configure-snip% cl 'hscroll "Horizontal Scroll" #t
					      void void)
	(inherit get-tagged-value)
	(public*
	 [get-hscroll
	  (lambda () (get-tagged-value 'hscroll))])
	(super-new))))

  (define gb:make-canvas-vscroll-checkable-snip%
    (lambda (cl)
      (class (gb:make-boolean-configure-snip% cl 'vscroll "Vertical Scroll" #t
					      void void)
	(inherit get-tagged-value)
	(public*
	 [get-vscroll
	  (lambda () (get-tagged-value 'vscroll))])
	(super-new))))

  (define gb:make-sb-box-snip%
    (lambda (cl item-kind)
      (class cl
	(inherit-field w h)
	(inherit get-hscroll get-vscroll)
	(field
	  [sb-width 10]
	  [canvas-min-space 15])
	(override*
	 [get-frame%
	  (lambda ()
	    (class (super get-frame%)
	     (override*
	      [get-kind (lambda () item-kind)])
	     (super-new)))]
	 [init-x-stretch? (lambda () #t)]
	 [init-y-stretch? (lambda () #t)]
	 [gb-get-min-size
	  (lambda (dc)
	    (values (+ sb-width canvas-min-space)
		    (+ sb-width canvas-min-space)))]
	 [draw
	  (lambda (dc x y . other)
	    (send dc draw-rectangle x y w h)
	    (when (get-vscroll)
	      (send dc draw-line 
		    (+ x w (- sb-width)) y
		    (+ x w (- sb-width)) (+ y h -1)))
	    (when (get-hscroll)
	      (send dc draw-line
		    x (+ y h (- sb-width))
		    (+ x w -1) (+ y h (- sb-width)))))])
	(super-new))))

  (define gb:make-canvas-snip%
    (lambda (cl cn)
      (class cl
	(inherit get-hscroll get-vscroll gb-get-instantiate-class-getter)
	(override*
	  [gb-get-style
	   (lambda ()
	     (append
	      (super gb-get-style)
	      (cond
	       [(and (get-hscroll) (get-vscroll)) '(hscroll vscroll)]
	       [(get-hscroll) '(hscroll)]
	       [(get-vscroll) '(vscroll)]
	       [else null])))]
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "canvas"))]
	  
	  [gb-get-default-class (lambda () 'canvas%)])
	(super-new))))
	  
  (define gb:canvas-snip% (gb:make-canvas-snip%
			   (gb:make-sb-box-snip%
			    (gb:make-canvas-vscroll-checkable-snip%
			     (gb:make-canvas-hscroll-checkable-snip%
			      gb:atomic-snip%))
			    "Canvas")
			   "gb:canvas"))
  
  (register-class gb:canvas-snip% "gb:canvas")

  (define gb:make-ecanvas-hscroll-select-snip%
    (lambda (cl)
      (class (gb:make-select-configure-snip% cl 'hscroll "Horizontal Scroll"
					     '("Show" "Hide" "No Scrolling"))
	(inherit get-tagged-value)
	(public*
	  [get-hscroll
	   (lambda () (zero? (get-hscroll-val)))]
	  [get-hscroll-val
	   (lambda () (get-tagged-value 'hscroll))])
	(super-new))))

  (define gb:make-ecanvas-vscroll-select-snip%
    (lambda (cl)
      (class (gb:make-select-configure-snip% cl 'vscroll "Vertical Scroll"
					     '("Show" "Hide" "No Scrolling"))
	(inherit get-tagged-value)
	(public*
	  [get-vscroll
	   (lambda () (zero? (get-vscroll-val)))]
	  [get-vscroll-val
	   (lambda () (get-tagged-value 'vscroll))])
	(super-new))))

  (define gb:make-editor-canvas-snip%
    (lambda (cl cn)
      (class cl
	(inherit get-hscroll-val get-vscroll-val)
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "ecanvas"))]
	  
	  [gb-get-default-class (lambda () 'editor-canvas%)]
	  [gb-get-style
	   (lambda ()
	     (append
	      (super gb-get-style)
	      (case (get-hscroll-val)
		[(0) null]
		[(1) '(hide-hscroll)]
		[(2) '(no-hscroll)])
	      (case (get-vscroll-val)
		[(0) ()]
		[(1) '(hide-vscroll)]
		[(2) '(no-vscroll)])))])
	(super-new))))
	  
  (define gb:editor-canvas-snip% (gb:make-editor-canvas-snip%
				 (gb:make-sb-box-snip%
				  (gb:make-ecanvas-vscroll-select-snip%
				   (gb:make-ecanvas-hscroll-select-snip%
				    gb:atomic-snip%))
				  "Editor Canvas")
				 "gb:editor-canvas"))
  
  (register-class gb:editor-canvas-snip% "gb:editor-canvas")

  (provide gb:canvas-snip%
	   gb:editor-canvas-snip%))
