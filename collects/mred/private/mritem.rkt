(module mritem racket/base
  (require mzlib/class
           mzlib/class100
           mzlib/list
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "const.rkt"
           "kw.rkt"
           "gdi.rkt"
           "check.rkt"
           "helper.rkt"
           "wx.rkt"
           "wxitem.rkt"
           "wxlitem.rkt"
           "mrwindow.rkt"
           "mrcontainer.rkt")

  (provide control<%>
           (protect-out control%-keywords
                        basic-control%)
           message%
           button%
           check-box%
           radio-box%
           slider%
           gauge%

           list-control<%>
           choice%
           list-box%

           (protect-out wrap-callback
                        check-list-control-args
                        check-list-control-selection
                        
                        ;; Local methods:
                        hidden-child?
                        label-checker))

  (define control<%>
    (interface (subwindow<%>)
      command))

  (define-local-member-name hidden-child? label-checker)

  (define-keywords control%-nofont-keywords
    window%-keywords
    subarea%-keywords
    area%-keywords)

  (define-keywords control%-keywords
    [font no-val]
    control%-nofont-keywords)

  (define basic-control%
    (class100* (make-subwindow% (make-window% #f (make-subarea% area%))) (control<%>) 
      (mk-wx mismatches lbl parent cb cursor
             ;; for keyword use
             [font no-val])
      (rename [super-set-label set-label])
      (private-field [label lbl][callback cb] 
                     [can-bitmap? (or (lbl . is-a? . wx:bitmap%)
                                      (pair? lbl))]
                     [can-string? (or (string? lbl)
                                      (pair? lbl))])
      (override
	[get-label (lambda () label)]
	[get-plain-label (lambda () 
                           (let ([label (if (pair? label) (cadr label) label)])
                             (and (string? label) (wx:label->plain-label label))))]
	[set-label (entry-point
		    (lambda (l)
		      ((label-checker)
		       '(method control<%> set-label) l)
		      (let ([l (if (string? l)
                                 (string->immutable-string l)
                                 l)])
                        (when (or (and can-bitmap?
                                       (l . is-a? . wx:bitmap%))
                                  (and can-string?
                                       (string? l)))
                          (send wx set-label l)
                          (if (pair? label)
                              (if (string? l)
                                  (set! label (list (car label) l (caddr label)))
                                  (set! label (list l (cadr label) (caddr label))))
                              (set! label l))))))])
      (public
	[hidden-child? (lambda () #f)] ; module-local method
	[label-checker  (lambda () check-label-string/false)] ; module-local method
	[command (lambda (e) (void (callback this e)))]) ; no entry/exit needed
      (private-field
       [wx #f])
      (sequence
	(when (string? label)
	  (set! label (string->immutable-string label)))
	(super-init (lambda () (set! wx (mk-wx)) wx) (lambda () wx) (lambda () wx) mismatches label parent cursor)
	(unless (hidden-child?)
	  (as-exit (lambda () (send parent after-new-child this)))))))

  (define (wrap-callback cb)
    (if (and (procedure? cb)
	     (procedure-arity-includes? cb 2))
	(lambda (w e) (if (or (eq? 'windows (system-type))
			      (and (memq (system-type) '(macos macosx))
				   (eq? (send e get-event-type) 'slider)))
			  ;; Mac OS slider and Windows (all): need trampoline
			  (wx:queue-callback
			   (lambda ()
			     (cb (wx->proxy w) e))
			   wx:middle-queue-key)
			  (cb (wx->proxy w) e)))
	cb))

  (define zero-bitmap #f)

  (define message%
    (class100*/kw basic-control% () [(label parent [style null]) control%-keywords [auto-resize #f]]
      (sequence ; abuse of `sequence'!
        (inherit/super [super-min-width min-width] 
                       [super-min-height min-height]
                       [super-get-label get-label]))
      (private-field
       [do-auto-resize? auto-resize]
       [orig-font (or (no-val->#f font)
                      normal-control-font)]
       [dx 0]
       [dy 0])
      (override
	[label-checker  (lambda () check-label-string-or-bitmap)] ; module-local method
      	[set-label (entry-point
		    (lambda (l)
                      (super set-label l)
                      (when do-auto-resize?
                        (do-auto-resize))))])
      (private
        [strip-amp (lambda (s) (if (string? s)
                                   (regexp-replace* #rx"&(.)" s "\\1")
                                   s))]
        [do-auto-resize (lambda ()
                          (let ([s (strip-amp (super-get-label))])
                            (cond
                             [(symbol? s) (void)]
                             [(string? s)
                              (let ([m (mred->wx this)])
                                (if (send m set-preferred-size)
                                    (let ([w (box 0)] [h (box 0)])
                                      (send m get-size w h)
                                      (super-min-width (unbox w))
                                      (super-min-height (unbox h)))
                                    (let-values ([(mw mh) (get-window-text-extent s orig-font #t)])
                                      (super-min-width (+ dx mw))
                                      (super-min-height (+ dy mh)))))]
                             [(s . is-a? . wx:bitmap%)
                              (super-min-width (+ dx (send s get-width)))
                              (super-min-height (+ dy (send s get-height)))])))])
      (public
        [(auto-resize-parm auto-resize)
         (case-lambda
          [() do-auto-resize?]
          [(on?) 
           (as-entry
            (lambda ()
              (set! do-auto-resize? (and #t))
              (when on?
                (do-auto-resize))))])])
      (sequence
	(let ([cwho '(constructor message)])
	  (check-label-string/bitmap/iconsym cwho label)
	  (check-container-parent cwho parent)
	  (check-style cwho #f '(deleted) style)
	  (check-font cwho font))
	(as-entry
	 (lambda ()
	   (super-init (lambda () 
                         (let ([m (make-object wx-message% this this
                                               (mred->wx-container parent)
                                               (if do-auto-resize? 
                                                   (cond
                                                    [(string? label) ""]
                                                    [(label . is-a? . wx:bitmap%)
                                                     (unless zero-bitmap
                                                       (set! zero-bitmap (make-object wx:bitmap% 1 1)))
                                                     zero-bitmap]
                                                    [else label])
                                                   label)
                                               -1 -1 style (no-val->#f font))])
                           ;; Record dx & dy:
                           (let ([w (box 0)] [h (box 0)])
                             (send m get-size w h)
                             (let-values ([(mw mh) (cond
                                                    [(string? label)
                                                     (let ([s (if do-auto-resize?
                                                                  ""
                                                                  (strip-amp label))]
                                                           [font orig-font])
                                                       (if (equal? s "")
                                                           (let-values ([(w h) (get-window-text-extent " " font)])
                                                             (values 0 h))
                                                           (get-window-text-extent s font)))]
                                                    [(label . is-a? . wx:bitmap%)
                                                     (if do-auto-resize?
                                                         (values 1 1)
                                                         (values (send label get-width)
                                                                 (send label get-height)))]
                                                    [else (values 0 0)])])
                               (set! dx (- (unbox w) mw))
                               (set! dy (- (unbox h) mh))))
                           ;; If auto-resize, install label now:
                           (when (and do-auto-resize?
                                      (not (symbol? label)))
                             (send m set-label label))
                           m))
		       (lambda () 
			 (let ([cwho '(constructor message)])
			   (check-container-ready cwho parent)))
		       label parent void #f)
           (when do-auto-resize?
             (do-auto-resize)))))))

  (define button%
    (class100*/kw basic-control% () [(label parent [callback (lambda (b e) (void))] [style null]) control%-keywords]
      (override
	[label-checker  (lambda () check-label-string-or-bitmap)]) ; module-local method
      (sequence
	(let ([cwho '(constructor button)])
	  (check-label-string-or-bitmap-or-both cwho label)
	  (check-container-parent cwho parent)
	  (check-callback cwho callback)
	  (check-style cwho #f '(border deleted) style)
	  (check-font cwho font))
	(as-entry
	 (lambda ()
	   (super-init (lambda () (make-object wx-button% this this
					       (mred->wx-container parent) (wrap-callback callback)
					       label -1 -1 -1 -1 style (no-val->#f font)))
		       (lambda () 
			 (let ([cwho '(constructor button)])
			   (check-container-ready cwho parent)))
		       label parent callback #f))))))

  (define check-box%
    (class100*/kw basic-control% () [(label parent [callback (lambda (b e) (void))] [style null] [value #f]) control%-keywords]
      (sequence
	(let ([cwho '(constructor check-box)])
	  (check-label-string-or-bitmap cwho label)
	  (check-container-parent cwho parent)
	  (check-callback cwho callback)
	  (check-style cwho #f '(deleted) style)
	  (check-font cwho font)))
      (override
	[label-checker  (lambda () check-label-string-or-bitmap)]) ; module-local method
      (private-field
       [wx #f])
      (public
	[get-value (entry-point (lambda () (send wx get-value)))]
	[set-value (entry-point (lambda (v) (send wx set-value v)))])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init (lambda () 
			 (set! wx (make-object wx-check-box% this this
					       (mred->wx-container parent) (wrap-callback callback)
					       label -1 -1 -1 -1 style (no-val->#f font)))
			 wx)
		       (lambda ()
			 (let ([cwho '(constructor check-box)])
			   (check-container-ready cwho parent)))
		       label parent callback #f)))
	(when value (set-value #t)))))

  (define radio-box%
    (class100*/kw basic-control% () 
		  [(label choices parent [callback (lambda (b e) (void))] [style '(vertical)] [selection 0]) control%-keywords]
      (private-field [chcs choices])
      (sequence 
	(let ([cwho '(constructor radio-box)])
	  (check-label-string/false cwho label)
	  (unless (and (list? chcs) (pair? chcs)
		       (or (andmap label-string? chcs)
			   (andmap (lambda (x) (is-a? x wx:bitmap%)) chcs)))
	    (raise-type-error (who->name cwho) "non-empty list of strings (up to 200 characters) or bitmap% objects" chcs))
	  (check-container-parent cwho parent)
	  (check-callback cwho callback)
	  (check-orientation cwho style)
	  (check-non-negative-integer/false cwho selection)))
      (private-field
       [wx #f])
      (private
	[check-button
	 (lambda (method n false-ok?)
	   ((if false-ok?
                check-non-negative-integer/false 
                check-non-negative-integer)
            `(method radio-box% ,method) n)
           (when n
             (unless (< n (length chcs))
               (raise-mismatch-error (who->name `(method radio-box% ,method)) "no such button: " n))))])
      (override
	[enable (entry-point
		 (case-lambda
		  [(on?) (send wx enable on?)]
		  [(which on?) (check-button 'enable which #f)
		   (send wx enable which on?)]))]
	[is-enabled? (entry-point
		      (case-lambda
		       [() (send wx is-enabled?)]
		       [(which) (check-button 'is-enabled? which #f)
			(send wx is-enabled? which)]))])
      (public
	[get-number (lambda () (length chcs))]
	[get-item-label (lambda (n) 
			  (check-button 'get-item-label n #f)
			  (list-ref chcs n))]
	[get-item-plain-label (lambda (n) 
				(check-button 'get-item-plain-label n #f)
				(wx:label->plain-label (list-ref chcs n)))]
	
	[get-selection (entry-point (lambda () (let ([v (send wx get-selection)])
                                                 (if (equal? v -1)
                                                     #f
                                                     v))))]
	[set-selection (entry-point
			(lambda (v) 
			  (check-button 'set-selection v #t)
			  (send wx set-selection (or v -1))))])
      (sequence
	(as-entry
	 (lambda ()
	   (when (andmap string? chcs)
	     (set! chcs (map string->immutable-string chcs)))
	   (super-init (lambda () 
			 (set! wx (make-object wx-radio-box% this this
					       (mred->wx-container parent) (wrap-callback callback)
					       label -1 -1 -1 -1 chcs 0 style (no-val->#f font)))
			 wx)
		       (lambda ()
			 (let ([cwho '(constructor radio-box)])
			   (check-container-ready cwho parent)
                           (when selection
                             (unless (< selection (length choices))
                               (raise-mismatch-error (who->name cwho)
                                                     (format "initial selection is too large, given only ~a choices: "
                                                             (length choices))
                                                     selection)))))
		       label parent callback #f)))
	(when (or (not selection) (positive? selection))
	  (set-selection selection)))))

  (define slider%
    (class100*/kw basic-control% ()
		  [(label min-value max-value parent [callback (lambda (b e) (void))] [init-value min-value] [style '(horizontal)])
		   control%-keywords]
      (private-field [minv min-value][maxv max-value])
      (sequence 
	(let ([cwho '(constructor slider)])
	  (check-label-string/false cwho label)
	  (check-slider-integer cwho minv)
	  (check-slider-integer cwho maxv)
	  (check-container-parent cwho parent) 
	  (check-callback cwho callback)
	  (check-slider-integer cwho init-value)
	  (check-style cwho '(vertical horizontal) '(plain vertical-label horizontal-label deleted) style)
	  (check-font cwho font)))
      (private-field
       [wx #f])
      (public
	[get-value (entry-point (lambda () (send wx get-value)))]
	[set-value (entry-point
		    (lambda (v)
		      (check-slider-integer '(method slider% set-value) v)
		      (unless (<= minv v maxv)
			(raise-mismatch-error (who->name '(method slider% set-value))
					      (format "slider's range is ~a to ~a; cannot set the value to: "
						      minv maxv)
					      v))
		      (send wx set-value v)))])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init (lambda () 
			 (set! wx (make-object wx-slider% this this
					       (mred->wx-container parent) (wrap-callback callback)
					       label init-value minv maxv style (no-val->#f font)))
			 wx)
		       (lambda () 
			 (let ([cwho '(constructor slider)])
			   (check-container-ready cwho parent)))
		       label parent callback #f))))))

  (define gauge%
    (class100*/kw basic-control% ()
		  [(label range parent [style '(horizontal)]) control%-keywords]
      (sequence 
	(let ([cwho '(constructor gauge)])
	  (check-label-string/false cwho label)
	  (check-container-parent cwho parent) 
	  (check-gauge-integer cwho range)
	  (check-orientation cwho style)))
      (private-field
       [wx #f])
      (public
	[get-value (entry-point (lambda () (send wx get-value)))]
	[set-value (entry-point
		    (lambda (v)
		      (check-range-integer '(method gauge% set-value) v)
		      (when (> v (send wx get-range))
			(raise-mismatch-error (who->name '(method gauge% set-value))
					      (format "gauge's range is 0 to ~a; cannot set the value to: "
						      (send wx get-range))
					      v))
		      (send wx set-value v)))]
	[get-range (entry-point (lambda () (send wx get-range)))]
	[set-range (entry-point
		    (lambda (v)
		      (check-gauge-integer '(method gauge% set-range) v)
		      (send wx set-range v)))])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init (lambda () 
			 (set! wx (make-object wx-gauge% this this
					       (mred->wx-container parent)
					       label range style (no-val->#f font)))
			 wx)
		       (lambda ()
			 (let ([cwho '(constructor gauge)])
			   (check-container-ready cwho parent)))
		       label parent void #f))))))

  ;; List controls ----------------------------------------

  (define list-control<%>
    (interface (control<%>)
      clear append
      get-number
      get-string find-string
      get-selection
      get-string-selection
      set-selection
      set-string-selection))

  (define (-1=>false v) (if (negative? v) #f v))

  (define-local-member-name 
    -append-list-string
    -set-list-strings
    -set-list-string
    -delete-list-item)

  (define basic-list-control%
    (class100* basic-control% (list-control<%>) (mk-wx mismatches label parent selection callback init-choices)
      (private-field
       [content (map string->immutable-string init-choices)])
      (public
	[(-append append) (entry-point (lambda (i) 
					 (check-label-string '(method list-control<%> append) i)
					 (-append-list-string i)
					 (send wx append i)))]
	[clear (entry-point (lambda () (send wx clear) (set! content null)))]
	[get-number (entry-point (lambda () (send wx number)))]
	[get-string (entry-point (lambda (n) (check-item 'get-string n) (list-ref content n)))]
	[get-selection (entry-point (lambda () (and (positive? (send wx number)) (-1=>false (send wx get-selection)))))]
	[get-string-selection (entry-point (lambda () (and (positive? (send wx number)) 
							   (let ([v (send wx get-selection)])
							     (if (= v -1)
								 #f
								 (list-ref content v))))))]
	[set-selection (entry-point (lambda (s) (check-item 'set-selection s) (send wx set-selection s)))]
	[set-string-selection (entry-point
			       (lambda (s)
				 (check-label-string '(method list-control<%> set-string-selection) s)
				 (let ([pos (do-find-string s)])
				   (if pos
				       (send wx set-selection pos)
				       (raise-mismatch-error (who->name '(method list-control<%> set-string-selection))
							     "no item matching the given string: " s)))))]
	[find-string (entry-point (lambda (x)
				    (check-label-string '(method list-control<%> find-string) x)
				    (do-find-string x)))]
	
	[-append-list-string (lambda (i)
			       (set! content (append content (list i))))]
	[-set-list-string (lambda (i s)
                            (set! content (let loop ([content content][i i])
                                            (if (zero? i)
                                                (cons (string->immutable-string s) (cdr content))
                                                (cons (car content) (loop (cdr content) (sub1 i)))))))]
	[-delete-list-item (lambda (pos)
                             (set! content (let loop ([content content][pos pos])
                                            (if (zero? pos)
                                                (cdr content)
                                                (cons (car content) (loop (cdr content) (sub1 pos)))))))]
	[-set-list-strings (lambda (l)
			     (set! content (map string->immutable-string l)))])
      (private-field
       [wx #f])
      (private
	[do-find-string
	 (lambda (s)
	   (let loop ([l content][pos 0])
	     (cond
	      [(null? l) #f]
	      [(string=? s (car l)) pos]
	      [else (loop (cdr l) (add1 pos))])))]
	[check-item
	 (lambda (method n)
	   (check-non-negative-integer `(method list-control<%> ,method) n)
	   (let ([m (send wx number)])
	     (unless (< n m)
	       (raise-mismatch-error (who->name `(method list-control<%> ,method)) 
				     (if (zero? m)
					 "control has no items; given index: " 
					 (format "control has only ~a items, indexed 0 to ~a; given out-of-range index: " 
						 m (sub1 m)))
				     n))))])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init (lambda () (set! wx (mk-wx)) wx) mismatches label parent callback #f)))
	(when selection
	  (set-selection selection)))))

  (define (check-list-control-args cwho label choices parent callback)
    (check-label-string/false cwho label)
    (unless (and (list? choices) (andmap label-string? choices))
      (raise-type-error (who->name cwho) "list of strings (up to 200 characters)" choices))
    (check-container-parent cwho parent)
    (check-callback cwho callback))

  (define (check-list-control-selection cwho choices selection)
    (unless (< selection (length choices))
      (raise-mismatch-error (who->name cwho)
			    (format "initial selection is too large, given only ~a choices: "
				    (length choices))
			    selection)))

  (define choice%
    (class100*/kw basic-list-control% ()
		  [(label choices parent [callback (lambda (b e) (void))] [style null] [selection 0])
		   control%-keywords]
      (sequence
	(let ([cwho '(constructor choice)])
	  (check-list-control-args cwho label choices parent callback)
	  (check-style cwho #f '(vertical-label horizontal-label deleted) style)
	  (check-non-negative-integer cwho selection)
	  (check-font cwho font))
	(super-init (lambda () (make-object wx-choice% this this
					    (mred->wx-container parent) (wrap-callback callback)
					    label -1 -1 -1 -1 choices style (no-val->#f font)))
		    (lambda ()
		      (let ([cwho '(constructor choice)])
			(check-container-ready cwho parent)
			(unless (= 0 selection)
			  (check-list-control-selection cwho choices selection))))
		    label parent 
		    (and (positive? selection) selection)
		    callback
		    choices))))

  (define list-box%
    (class100*/kw basic-list-control%  ()
		  [(label choices parent [callback (lambda (b e) (void))] [style '(single)] [selection #f] [font no-val] [label-font no-val])
		   control%-nofont-keywords
                   [columns (list "Column")]
                   [column-order #f]]
      (sequence 
	(let ([cwho '(constructor list-box)])
	  (check-list-control-args cwho label choices parent callback)
	  (check-style cwho '(single multiple extended) 
                       '(vertical-label horizontal-label deleted variable-columns
                                        column-headers clickable-headers reorderable-headers) 
                       style)
	  (check-non-negative-integer/false cwho selection)
	  (check-font cwho font)
	  (check-font cwho label-font)
          (unless (and (list? columns)
                       (not (null? columns))
                       (andmap label-string? columns))
            (raise-type-error (who->name cwho) "non-empty list of strings (up to 200 characters)" columns))
          (when column-order
            (check-column-order cwho column-order (length columns)))))
      (private
        [check-column-order
         (lambda (cwho column-order count)
           (unless (and (list? column-order)
                        (andmap exact-integer? column-order)
                        (equal? (sort column-order <)
                                (for/list ([i (in-range (length column-order))]) i)))
             (raise-type-error (who->name cwho) 
                               "#f or list of distinct exact integers from 0 to one less than the list length" 
                               column-order))
           (unless (= (length column-order) count)
             (raise-mismatch-error (who->name cwho) 
                                   (format "column count ~a does not match length of column-order list: "
                                           count)
                                   column-order)))]
        [check-column-number
         (lambda (who i)
           (unless (exact-nonnegative-integer? i)
             (raise-type-error (who->name who) "exact nonnegative integer" i))
           (unless (i . < . num-columns)
             (raise-mismatch-error (who->name who) 
                                   (format
                                    "index is too large for ~a-column list box: "
                                    num-columns)
                                   i)))])
      (private-field
       [column-labels (map string->immutable-string columns)]
       [num-columns (length columns)]
       [variable-columns? (memq 'variable-columns style)])
      (rename [super-append append])
      (override
	[(-append append)
         (entry-point
          (case-lambda
           [(i) 
            (super-append i)]
           [(i d) 
            (check-label-string '(method list-control<%> append) i)
            (send this -append-list-string i)
            (send wx append i d)]))])
      (public
        [get-column-labels (lambda () column-labels)]
        [get-column-order (lambda () (send wx get-column-order))]
        [set-column-order (lambda (co) 
                            (check-column-order '(method list-box% set-column-order) co num-columns)
                            (send wx set-column-order co))]
        [set-column-label (lambda (i str)
                            (let ([who '(method list-box% set-column-label)])
                              (check-column-number who i)
                              (check-label-string who str))
                            (let ([str (string->immutable-string str)])
                              (set! column-labels (let loop ([i i] [l column-labels])
                                                    (cond
                                                     [(zero? i) (cons str (cdr l))]
                                                     [else (cons (car l) (loop (sub1 i) (cdr l)))])))
                              (send wx set-column-label i str)))]
	[set-column-width (lambda (i w min-size max-size) 
                            (let ([who '(method list-box% set-column-width)])
                              (check-column-number who i)
                              (check-dimension who w)
                              (check-dimension who min-size)
                              (check-dimension who max-size)
                              (unless (<= min-size w)
                                (raise-mismatch-error (who->name who)
                                                      (format
                                                       "size ~a is less than mininum size: "
                                                       w)
                                                      min-size))
                              (unless (>= max-size w)
                                (raise-mismatch-error (who->name who)
                                                      (format
                                                       "size ~a is less than maximum size: "
                                                       w)
                                                      max-size)))
                            (send wx set-column-size i w min-size max-size))]
        [get-column-width (lambda (i)
                           (check-column-number '(method list-box% get-column-width) i)
                           (send wx get-column-size i))]
        [delete-column (lambda (i)
                         (let ([who '(method list-box% delete-column)])
                           (check-column-number who i)
                           (unless variable-columns?
                             (raise-mismatch-error 
                              (who->name who)
                              "list box without 'variable-columns style cannot delete column: "
                              i))
                           (unless (num-columns . > . 1)
                             (raise-mismatch-error (who->name who)
                                                   "cannot delete only column: "
                                                   i)))
                         (as-entry
                          (lambda ()
                            (set! num-columns (sub1 num-columns))
                            (set! column-labels (let loop ([i i] [l column-labels])
                                                  (cond
                                                   [(zero? i) (cdr l)]
                                                   [else (cons (car l) (loop (sub1 i) (cdr l)))])))
                            (send wx delete-column i))))]
        [append-column (lambda (label)
                         (let ([who '(method list-box% append-column)])
                           (check-label-string who label)
                           (unless variable-columns?
                             (raise-mismatch-error 
                              (who->name who)
                              "list box without 'variable-columns style cannot add column: "
                              label)))
                         (as-entry
                          (lambda ()
                            (set! num-columns (add1 num-columns))
                            (set! column-labels (append column-labels (list label)))
                            (send wx append-column label))))]

        [delete (entry-point (lambda (n) 
			       (check-item 'delete n) 
			       (send this -delete-list-item n)
			       (send wx delete n)))]
	[get-data (entry-point (lambda (n) (check-item 'get-data n) (send wx get-data n)))]
	[get-label-font (lambda () (send wx get-label-font))]
	[get-selections (entry-point (lambda () (send wx get-selections)))]
	[number-of-visible-items (entry-point (lambda () (send wx number-of-visible-items)))]
	[is-selected? (entry-point (lambda (n) (check-item 'is-selected? n) (send wx selected? n)))]
	[set (entry-point (lambda (l . more) 
                            (let ([cwho '(method list-box% set)])
                              (unless (= num-columns (+ 1 (length more)))
                                (raise-mismatch-error (who->name cwho) 
                                                      (format
                                                       "column count ~a doesn't match number of arguments: "
                                                       num-columns)
                                                      (add1 (length more))))
                              (for ([l (in-list (cons l more))])
                                (unless (and (list? l) (andmap label-string? l))
                                  (raise-type-error (who->name cwho) 
                                                    "list of strings (up to 200 characters)" l)))
                              (for ([more-l (in-list more)])
                                (unless (= (length more-l) (length l))
                                  (raise-mismatch-error 
                                   (who->name cwho)
                                   (format "first list length ~a does not match length of later argument: "
                                           (length l))
                                   more-l))))
			    (send this -set-list-strings l)
			    (send wx set l . more)))]
	[set-string (entry-point
		     (lambda (n d [col 0])
                       (let ([cwho '(method list-box% set-string)])
                         (check-non-negative-integer cwho n) ; int error before string
                         (check-label-string cwho d) ; string error before range mismatch
                         (unless (exact-nonnegative-integer? col)
                           (raise-type-error (who->name cwho) "exact nonnegative integer" col))
                         (unless (< -1 col num-columns)
                           (raise-mismatch-error (who->name cwho) 
                                                 (format 
                                                  "column number is not in the list box's allowed range [0, ~a]: " 
                                                  (sub1 num-columns))
                                                 col)))
		       (check-item 'set-string n)
		       (send this -set-list-string n d)
		       (send wx set-string n d col)))]
	[set-data (entry-point (lambda (n d) (check-item 'set-data n) (send wx set-data n d)))]
	[get-first-visible-item (entry-point (lambda () (send wx get-first-item)))]
	[set-first-visible-item (entry-point (lambda (n) 
					       (check-item 'set-first-visible-item n) 
					       (send wx set-first-visible-item n)))]
	[select (entry-point 
		 (case-lambda 
		  [(n) (check-item 'select n) (send wx select n #t)]
		  [(n on?) (check-item 'select n) (send wx select n on?)]))])
      (private-field
       [wx #f])
      (private
	[check-item
	 (entry-point
	  (lambda (method n)
	    (check-non-negative-integer `(method list-box% ,method) n)
	    (let ([m (send wx number)])
	      (unless (< n m)
		(raise-mismatch-error (who->name `(method list-box% ,method))
				      (if (zero? m)
					  "list has no items; given index: " 
					  (format "list has only ~a items, indexed 0 to ~a; given out-of-range index: "
						  m (sub1 m)))
				      n)))))])
      (sequence
	(super-init (lambda () 
		      (let-values ([(kind style)
				    (cond
				     [(memq 'single style) (values 'single (remq 'single style))]
				     [(memq 'multiple style) (values 'multiple (remq 'multiple style))]
				     [else (values 'extended (remq 'extended style))])])
			(set! wx (make-object wx-list-box% this this
					      (mred->wx-container parent) (wrap-callback callback)
					      label kind
					      -1 -1 -1 -1 choices style 
					      (no-val->#f font) (no-val->#f label-font)
                                              column-labels
                                              column-order)))
		      wx)
		    (lambda ()
		      (let ([cwho '(constructor list-box)])
			(check-container-ready cwho parent)
			(when selection
			  (check-list-control-selection cwho choices selection))))
		    label parent (and (pair? choices) selection) callback
		    choices)))))
