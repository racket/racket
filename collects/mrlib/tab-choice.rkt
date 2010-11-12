
;; This code is not yet complete...

;; !!!!!! FIXME: code copied from framework !!!!!!
;;  -- The copy in framework should be removed --

(module tab-choice mzscheme
  (require mzlib/class
	   mred
	   mzlib/list)

  (provide auto-tab-panel%
	   single<%> single-mixin)
  
  (define single<%> (interface (area-container<%>) active-child))
  (define single-mixin
    (mixin (area-container<%>) (single<%>)
	   (inherit get-alignment change-children)
	   (define/override (after-new-child c)
	     (unless (is-a? c window<%>)

	       ;; would like to remove the child here, waiting on a PR submitted
	       ;; about change-children during after-new-child
	       (change-children
		(lambda (l)
		  (remq c l)))

	       (error 'single-mixin::after-new-child
		      "all children must implement window<%>, got ~e"
		      c))
	     (if current-active-child
		 (send c show #f)
		 (set! current-active-child c)))
	   [define/override (container-size l)
	     (if (null? l)
		 (values 0 0)
		 (values (apply max (map car l))
			 (apply max (map cadr l))))]
	   [define/override (place-children l width height)
	     (let-values ([(h-align-spec v-align-spec) (get-alignment)])
	       (let ([align
		      (lambda (total-size spec item-size)
			(floor
			 (case spec
			   [(center) (- (/ total-size 2) (/ item-size 2))]
			   [(left top) 0]
			   [(right bottom) (- total-size item-size)]
			   [else (error 'place-children
					"alignment spec is unknown ~a\n" spec)])))])
		 (map (lambda (l) 
			(let*-values ([(min-width min-height v-stretch? h-stretch?)
				       (apply values l)]
				      [(x this-width)
				       (if h-stretch?
					   (values 0 width)
					   (values (align width h-align-spec min-width)
						   min-width))]
				      [(y this-height)
				       (if v-stretch?
					   (values 0 height)
					   (values (align height v-align-spec min-height)
						   min-height))])
			  (list x y this-width this-height)))
		      l)))]

	   (inherit get-children begin-container-sequence end-container-sequence)
	   [define current-active-child #f]
	   (define/public active-child
	     (case-lambda
	      [() current-active-child]
	      [(x) 
	       (unless (memq x (get-children))
		 (error 'active-child "got a panel that is not a child: ~e" x))
	       (unless (eq? x current-active-child)
		 (begin-container-sequence)
		 (for-each (lambda (x) (send x show #f))
			   (get-children))
		 (set! current-active-child x)
		 (send current-active-child show #t)
		 (end-container-sequence))]))
	   (super-instantiate ())))

  (define auto-tab-panel%
    (class tab-panel%
      (init choices
	    parent 
	    [callback (lambda (b e) (void))])

      (inherit get-selection)

      (super-new [choices choices]
		 [parent parent]
		 [callback
		  (lambda (b e)
		    (send single-panel active-child (list-ref panels (get-selection)))
		    (callback b e))])

      (define single-panel (new (single-mixin pane%)
				[parent this]))

      (inherit get-number)
      (define panels (let loop ([i (get-number)])
		       (unless (zero? i)
			 (cons (new vertical-panel% [parent single-panel])
			       (loop (sub1 i))))))

      (define/public (get-panel i)
	(list-ref panels i))

      (define/public active-child
	(case-lambda
	 [() (send single-panel active-child)]
	 [(c)
	  (send single-panel active-child c)
	  (let loop ([i 0][l panels])
	    (unless (null? l)
	      (if (eq? (car l) c)
		  (super set-selection i)
		  (loop (add1 i) (cdr l)))))]))

      (define/override (set-selection i)
	(super set-selection i)
	(send single-panel active-child (list-ref panels i))))))
