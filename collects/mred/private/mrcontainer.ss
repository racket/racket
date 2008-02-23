(module mrcontainer mzscheme
  (require mzlib/class
	   mzlib/class100
	   mzlib/list
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "helper.ss"
	   "const.ss"
	   "wx.ss"
	   "kw.ss"
	   "check.ss"
	   "wxcontainer.ss"
	   "mrwindow.ss")

  (provide area-container<%>
	   (protect internal-container<%>
		    check-container-parent
		    container%-keywords
		    make-container%)
	   area-container-window<%>
	   (protect make-area-container-window%))

  (define area-container<%> 
    (interface (area<%>) 
      reflow-container container-flow-modified begin-container-sequence end-container-sequence
      container-size
      get-children change-children place-children
      after-new-child
      add-child delete-child
      border spacing 
      set-alignment get-alignment))

  (define internal-container<%> (interface ()))

  (define (check-container-parent who p)
    (unless (is-a? p internal-container<%>)
      (raise-type-error (who->name who) "built-in container<%> object" p)))
  
  (define-keywords container%-keywords
    [border no-val]
    [spacing no-val]
    [alignment no-val])

  (define (make-container% %) ; % implements area<%>
    (class100* % (area-container<%> internal-container<%>) (mk-wx get-wx-pan mismatches parent
								  ;; for keyword use
								  [border no-val]
								  [spacing no-val]
								  [alignment no-val])
      (sequence
	(let ([cwho '(iconstructor area-container)])
	  (unless (eq? border no-val) (check-margin-integer cwho border))
	  (unless (eq? spacing no-val) (check-margin-integer cwho spacing))
	  (unless (eq? alignment no-val)
	    (unless (and (list? alignment)
			 (= 2 (length alignment))
			 (memq (car alignment) '(left center right))
			 (memq (cadr alignment) '(top center bottom)))
	      (raise-type-error (who->name cwho) "alignment list" alignment)))))
      (private-field [get-wx-panel get-wx-pan])
      (public
	[after-new-child (lambda (c) 
			   (check-instance '(method area-container<%> after-new-child) subarea<%> 'subarea<%> #f c)
			   (void))]
	[reflow-container (entry-point (lambda () (send (send (get-wx-panel) get-top-level) force-redraw)))]
	[container-flow-modified (entry-point (lambda () 
						(let ([p (get-wx-panel)])
						  (send p need-move-children)
						  (send p force-redraw))))]
	[begin-container-sequence (entry-point (lambda () (send (send (get-wx-panel) get-top-level) begin-container-sequence)))]
	[end-container-sequence (entry-point (lambda () (send (send (get-wx-panel) get-top-level) end-container-sequence)))]
	[get-children (entry-point (lambda () (map wx->proxy 
						   (let ([l (send (get-wx-panel) get-children)]
							 [h (send (get-wx-panel) get-hidden-child)])
						     (if h (remq h l) l)))))]
	[(bdr border) (param get-wx-panel border)]
	[(spc spacing) (param get-wx-panel spacing)]
	[set-alignment (entry-point (lambda (h v) (send (get-wx-panel) alignment h v)))]
	[get-alignment (entry-point (lambda () (send (get-wx-panel) get-alignment)))]
	[change-children (entry-point
			  (lambda (f)
			    (unless (and (procedure? f)
					 (procedure-arity-includes? f 1))
			      (raise-type-error (who->name '(method container<%> change-children))
						"procedure or arity 1"
						f))
			    (send (get-wx-panel) change-children
				  (lambda (kids)
				    (let* ([hidden (send (get-wx-panel) get-hidden-child)]
					   [mred-kids (map wx->proxy (remq hidden kids))]
					   [l (as-exit (lambda () (f mred-kids)))])
				      (unless (and (list? l)
						   (andmap (lambda (x) (is-a? x internal-subarea<%>)) l))
					(raise-mismatch-error 'change-children
							      "result of given procedure was not a list of subareas: "
							      l))
				      (append
				       (if hidden (list hidden) null)
				       (map mred->wx l)))))))]
	[container-size (entry-point
			 (lambda (l) 
					; Check l, even though we don't use it
			   (unless (and (list? l) 
					(andmap
					 (lambda (l)
					   (and (list? l) (= (length l) 4)
						(integer? (car l)) (exact? (car l)) (<= 0 (car l) 10000)
						(integer? (cadr l)) (exact? (cadr l)) (<= 0 (cadr l) 10000)))
					 l))
			     (raise-type-error (who->name '(method area-container<%> container-size))
					       "list of lists containing two exact integers in [0, 10000] and two booleans"
					       l))
			   (let ([l (send (get-wx-panel) do-get-graphical-min-size)])
			     (apply values l))))]
	[place-children (entry-point (lambda (l w h) (send (get-wx-panel) do-place-children l w h)))]
	[add-child (entry-point
		    (lambda (c) 
		      (check-instance '(method area-container<%> add-child) subwindow<%> 'subwindow<%> #f c)
		      (send (get-wx-panel) add-child (mred->wx c))))]
	[delete-child (entry-point
		       (lambda (c) 
			 (check-instance '(method area-container<%> delete-child) subwindow<%> 'subwindow<%> #f c)
			 (send (get-wx-panel) delete-child (mred->wx c))))])
      (sequence
	(super-init mk-wx get-wx-panel mismatches parent)
	(unless (eq? border no-val) (bdr border))
	(unless (eq? spacing no-val) (spc spacing))
	(unless (eq? alignment no-val) (set-alignment . alignment)))))

  (define area-container-window<%>
    (interface (window<%> area-container<%>)))

  (define (make-area-container-window% %) ; % implements window<%> (and area-container<%>)
    (class100* % (area-container-window<%>) (mk-wx get-wx-pan mismatches label parent cursor) 
      (private-field [get-wx-panel get-wx-pan])
      (sequence
	(super-init mk-wx get-wx-panel mismatches label parent cursor)))))


