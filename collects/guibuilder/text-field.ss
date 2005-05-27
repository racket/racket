
(module text-field mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss"
	   "base.ss"
	   "feature.ss")

  (define gb:make-text-initial-snip%
    (lambda (cl)
      (class cl
	(inherit gb-need-recalc-size get-style)
	(private-field
	  [initial "value"])
	(public*
	  [get-initial (lambda () initial)]
	  [get-initial-size
	   (lambda (dc)
	     (let-values ([(w h d a) (send dc get-text-extent initial
					   (send (get-style) get-font))])
	       (values w h)))]

	  [initial-install
	   (lambda (i)
	     (set! initial i))])
	(override*
	  [get-frame%
	   (lambda ()
	     (class (super get-frame%)
	       (inherit-field controls)
	       (super-new)
	       (private-field
		 [initial-text
		  (make-one-line/callback-edit controls "Initial:"
					       (lambda (txt)
						 (set! initial txt)
						 (gb-need-recalc-size))
					       initial)])))]

	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o initial-install initial)
	       o))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (send stream put (string->bytes/utf-8 initial)))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (initial-install ((get-bytes->string version) (send stream get-bytes))))])
	(super-new))))

  (define gb:make-text-hscroll-checkable-snip%
    (lambda (cl)
      (class (gb:make-boolean-configure-snip% cl 'hscroll "Horizontal Scroll" #f 
					      void
					      (lambda (f snip)
						(send (send f find-control 'hscroll)
						      enable
						      (send snip get-tagged-value 'multi))))
	(inherit get-tagged-value)
	(override*
	  [gb-get-style
	   (lambda () 
	     (append
	      (if (get-tagged-value 'hscroll)
		  '(hscroll)
		  null)
	      (super gb-get-style)))])
	(super-new))))
  
  (define gb:make-text-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h)
	(inherit get-initial-size get-initial
		 get-callback-names get-multi
		 get-label)
	(private-field
	  [margin 2])
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "text"))]
	  [init-x-stretch? (lambda () #t)]
	  [get-label-top-margin (lambda () margin)]
	  [get-min-body-size
	   (lambda (dc)
	     (let-values ([(w h) (get-initial-size dc)])
		(values (+ w (* 2 margin))
			(+ (* h (if (get-multi) 3 1))
			   (* 2 margin)))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (send dc draw-text (get-initial) (+ x margin) (+ y margin)))]
	  [get-callback-kinds (lambda ()
				(list "-change-callback" "-return-callback" "-focus-callback"))]
	  [gb-get-default-class (lambda () 'text-field%)]
	  [gb-get-style (lambda () (append
				    (super gb-get-style)
				    (if (get-multi) '(multiple) '(single))))]
	  [gb-get-unified-callback
	   (lambda ()
	     (let-values ([(change return focus) 
			   (apply values (get-callback-names))])
	       `(lambda (b e) 
		  (let ([t (send e get-event-type)])
		    (cond 
		     [(eq? t 'text-field) (,change b e)]
		     [(eq? t 'text-field-enter) (,return b e)]
		     [else (,focus b e)])))))]
	  [gb-instantiate-arguments
	   (lambda ()
	     (cons
	      `[init-value ,(get-initial)]
	      (super gb-instantiate-arguments)))])
	(super-new))))
  
  (define gb:text-snip% (gb:make-text-snip%
			 (gb:make-text-hscroll-checkable-snip%
			  (gb:make-multi-checkable-snip%
			   (gb:make-text-initial-snip%
			    (gb:make-callback-snip%
			     (gb:make-text-labelled-snip% gb:atomic-snip% 
							  "Text")))))
			 "gb:text"))
  
  (register-class gb:text-snip% "gb:text")
  
  (provide gb:text-snip%))
