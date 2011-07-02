(module mrpanel mzscheme
  (require mzlib/class
           mzlib/class100
           (prefix wx: "kernel.rkt")
           "lock.rkt"
           "const.rkt"
           "check.rkt"
           "helper.rkt"
           "wx.rkt"
           "kw.rkt"
           "wxpanel.rkt"
           "mrwindow.rkt"
           "mrcontainer.rkt")

  (provide pane%
           vertical-pane%
           horizontal-pane%
           grow-box-spacer-pane%
           panel%
           vertical-panel%
           horizontal-panel%
           tab-panel%
           group-box-panel%)

  (define-keywords pane%-keywords
    subarea%-keywords
    container%-keywords
    area%-keywords)

  (define-local-member-name get-initial-label)

  (define pane%
    (class100*/kw (make-subarea% (make-container% area%)) ()
		  [(parent) pane%-keywords]
      (private-field [wx #f])
      (sequence 
	(let* ([who (cond ; yuck! - we do this to make h-p and v-p subclasses of p
		     [(is-a? this vertical-pane%) 'vertical-pane]
		     [(is-a? this horizontal-pane%) 'horizontal-pane]
		     [(is-a? this grow-box-spacer-pane%) 'grow-box-spacer-pane]
		     [else 'pane])]
	       [cwho `(constructor ,who)])
	  (check-container-parent cwho parent)
	  (as-entry
	   (lambda ()
	     (super-init (lambda ()
                           (set! wx (make-object (case who
                                                   [(vertical-pane) wx-vertical-pane%]
                                                   [(horizontal-pane) wx-horizontal-pane%]
                                                   [(grow-box-spacer-pane) wx-grow-box-pane%]
                                                   [else wx-pane%])
                                                 this this (mred->wx-container parent) null
                                                 #f))
                           wx)
			 (lambda () wx)
			 (lambda () wx)
			 (lambda ()
			   (check-container-ready cwho parent))
			 parent)
	     (send (send wx area-parent) add-child wx)))
	  (send parent after-new-child this)))))

  (define vertical-pane% (class100*/kw pane% () [(parent) pane%-keywords] (sequence (super-init parent))))
  (define horizontal-pane% (class100*/kw pane% () [(parent) pane%-keywords] (sequence (super-init parent))))
  (define grow-box-spacer-pane% (class100*/kw pane% () [(parent) pane%-keywords] (sequence (super-init parent))))

  (define-keywords panel%-keywords
    window%-keywords
    subarea%-keywords
    container%-keywords
    area%-keywords)

  (define panel%
    (class100*/kw (make-subwindow%
                   (make-area-container-window% 
                    (make-window% #f (make-subarea% (make-container% area%)))) )
                  (subwindow<%>)
		  [(parent [style null]) panel%-keywords]
      (private-field [wx #f])
      (public [get-initial-label (lambda () #f)])
      (sequence 
	(let* ([who (cond ; yuck! - we do this to make h-p and v-p subclasses of p
		     [(is-a? this tab-panel%) 'tab-panel]
		     [(is-a? this group-box-panel%) 'group-box-panel]
		     [(is-a? this vertical-panel%) 'vertical-panel]
		     [(is-a? this horizontal-panel%) 'horizontal-panel]
		     [else 'panel])]
	       [cwho `(constructor ,who)]
               [can-canvas? (memq who '(vertical-panel
                                        horizontal-panel
                                        panel))]
               [as-canvas? (lambda () (or (memq 'vscroll style)
                                          (memq 'auto-vscroll style)
                                          (memq 'hscroll style)
                                          (memq 'auto-hscroll style)))])
	  (check-container-parent cwho parent)
	  (check-style cwho #f (append '(border deleted)
                                       (if can-canvas? 
                                           '(hscroll vscroll auto-hscroll auto-vscroll)
                                           null)) 
                       style)
	  (as-entry
	   (lambda ()
	     (super-init (lambda () (set! wx (make-object (case who
							    [(vertical-panel) 
                                                             (if (as-canvas?)
                                                                 wx-vertical-canvas-panel%
                                                                 wx-vertical-panel%)]
							    [(tab-panel) wx-vertical-tab-panel%]
							    [(group-box-panel) wx-vertical-group-panel%]
							    [(horizontal-panel) 
                                                             (if (as-canvas?)
                                                                 wx-horizontal-canvas-panel%
                                                                 wx-horizontal-panel%)]
							    [else (if (as-canvas?)
                                                                      wx-canvas-panel%
                                                                      wx-panel%)])
							  this this (mred->wx-container parent) 
                                                          (cons 'transparent style)
                                                          (get-initial-label)))
                                 wx)
			 (lambda () wx) 
			 (lambda () wx) 
			 (lambda () (check-container-ready cwho parent))
			 #f parent #f)
	     (unless (memq 'deleted style)
	       (send (send wx area-parent) add-child wx))))
	  (send parent after-new-child this)))))

  (define vertical-panel% 
    (class100*/kw panel% () 
                  [(parent [style null]) panel%-keywords] 
                  (sequence (super-init parent style))
                  (public [set-orientation (λ (x) (send (mred->wx this) set-orientation x))]
                          [get-orientation (λ () (send (mred->wx this) get-orientation))])))
  (define horizontal-panel% 
    (class100*/kw panel% () 
                  [(parent [style null]) panel%-keywords] 
                  (sequence (super-init parent style))
                  (public [set-orientation (λ (x) (send (mred->wx this) set-orientation x))]
                          [get-orientation (λ () (send (mred->wx this) get-orientation))])))

  (define list-append append)

  (define tab-panel%
    (class100*/kw vertical-panel% ()
		  [(choices parent [callback (lambda (b e) (void))] [style null] [font no-val]) panel%-keywords]
      (private-field [save-choices choices])
      (override [get-initial-label (lambda () save-choices)])

      (sequence
	(let ([cwho '(constructor tab-panel)])
	  (unless (and (list? choices) (andmap label-string? choices))
	    (raise-type-error (who->name cwho) "list of strings (up to 200 characters)" choices))
	  (check-callback cwho callback)
	  (check-container-parent cwho parent)
	  (check-style cwho #f '(deleted no-border) style)
	  (check-font cwho font))
	(super-init parent (if (memq 'no-border style)
                               (if (eq? (car style) 'no-border)
                                   (cdr style)
                                   (list (car style)))
                               (cons 'border style)))
        (send (mred->wx this) set-callback (lambda (wx e) (callback (wx->mred wx) e))))

      (public
	[get-number (lambda () (length save-choices))]
	[append (entry-point
		 (lambda (n)
		   (check-label-string '(method tab-panel% append) n)
		   (let ([n (string->immutable-string n)])
		     (set! save-choices (list-append save-choices (list n)))
		     (send (mred->wx this) append n))))]
	[get-selection (lambda () (and (pair? save-choices)
				       (send (mred->wx this) get-selection)))]
	[set-selection (entry-point
			(lambda (i)
			  (check-item 'set-selection i)
			  (send (mred->wx this) set-selection i)))]
	[delete (entry-point
		 (lambda (i)
		   (check-item 'delete i)
		   (set! save-choices (let loop ([p 0][l save-choices])
					(if (= p i)
					    (cdr l)
					    (cons (car l) (loop (add1 p) (cdr l))))))
		   (send (mred->wx this) delete i)))]
	[set-item-label (entry-point
			 (lambda (i s)
			   (check-item 'set-item-label i)
			   (check-label-string '(method tab-panel% set-item-label) s)
			   (let ([s (string->immutable-string s)])
                             (set! save-choices (let loop ([save-choices save-choices][i i])
                                                  (if (zero? i)
                                                      (cons s (cdr save-choices))
                                                      (cons (car save-choices) (loop (cdr save-choices) (sub1 i))))))
			     (send (mred->wx this) set-label i s))))]
	[set
	 (entry-point (lambda (l) 
			(unless (and (list? l) (andmap label-string? l))
			  (raise-type-error (who->name '(method tab-panel% set)) 
					    "list of strings (up to 200 characters)" l))
			(set! save-choices (map string->immutable-string l))
			(send (mred->wx this) set l)))]
	[get-item-label (entry-point
			 (lambda (i)
			   (check-item 'get-item-label i)
			   (list-ref save-choices i)))])
      
      (private
	[check-item
	 (lambda (method n)
	   (check-non-negative-integer `(method tab-panel% ,method) n)
	   (let ([m (length save-choices)])
	     (unless (< n m)
	       (raise-mismatch-error (who->name `(method tab-panel% ,method)) 
				     (if (zero? m)
					 "panel has no tabs; given index: " 
					 (format "panel has only ~a tabs, indexed 0 to ~a; given out-of-range index: " 
						 m (sub1 m)))
				     n))))])))

  (define group-box-panel%
    (class100*/kw vertical-panel% ()
		  [(label parent [style null] [font no-val]) panel%-keywords]
      (private-field
       [lbl label])
      (override [get-initial-label (lambda () lbl)])

      (sequence
	(let ([cwho '(constructor group-box-panel)])
	  (check-label-string cwho label)
	  (check-container-parent cwho parent)
	  (check-style cwho #f '(deleted) style)
	  (check-font cwho font))

	;; Technically a bad way to change margin defaults, since it's
	;;  implemented with an update after creation:
	(when (eq? horiz-margin no-val) (set! horiz-margin 2))
	(when (eq? vert-margin no-val) (set! vert-margin 2))

	(super-init parent (if (memq 'deleted style)
                               '(deleted)
                               null)))

      (override
	[set-label (entry-point
		    (lambda (s)
		      (check-label-string '(method group-box-panel% set-label) s)
		      (set! lbl (if (immutable? s)
				    s
				    (string->immutable-string s)))
		      (send (mred->wx this) set-label s)))]
	[get-label (lambda () lbl)]))))
