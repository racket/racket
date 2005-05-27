
(module base mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss")

  (define GB:SNIP-VERSION 5)
  (define MINOR-VERSION 0)

  ;; Info about the output mode:
  (define-struct output-mode (as-class? no-free-vars?))

  (define gb:snip%
    (class mred:snip% 
      (init-field [lm 5][tm 5][rm 5][bm 5])
      (inherit get-admin set-snipclass set-count)
      (private-field
	(need-recalc? #t)
	(prev-min-w 0)
	(prev-min-h 0))
      (field
       [x 0]
       [stable-x 0]
       [y 0]
       [stable-y 0]
       [w (+ lm rm)]
       [h (+ tm bm)]
       [spacing 3]
       [hilited? #f])
      (public*
       [spacing-+
	(lambda args
	  (+ (apply + args)
	     (let ([c (let loop ([l args])
			(cond
			 [(null? l) 0]
			 [(zero? (car l)) (loop (cdr l))]
			 [else (add1 (loop (cdr l)))]))])
	       (if (positive? c)
		   (* spacing (sub1 c))
		   0))))])
      (public*
       [init-horizontal-child-alignment (lambda () 2)]
       [init-vertical-child-alignment (lambda () 2)]
       [init-name (lambda () (new-name "item"))]
       [get-classname (lambda () "gb:core")]
       [container? (lambda () #t)]
       [init-y-stretch? (lambda () #t)]
       [init-x-stretch? (lambda () #t)])
      (field
       [horizontal-child-alignment (init-horizontal-child-alignment)]
       [vertical-child-alignment (init-vertical-child-alignment)]
       [with-border? #f]
       [dialog #f]
       [name (init-name)]
       [id #f]
       [original-id #f]
       [original-children-ids #f]
       (parent #f)
       (pb #f)
       (children null)
       (y-stretch? (init-y-stretch?))
       (x-stretch? (init-x-stretch?)))
      (public*
       (set-id (lambda (x) (set! id x)))
       (set-horizontal-child-alignment
	(lambda (v) (set! horizontal-child-alignment v)))
       (set-vertical-child-alignment
	(lambda (v) (set! vertical-child-alignment v)))
       (set-with-border
	(lambda (v) (set! with-border? v)))

       (get-frame%
	(lambda ()
	  (class mred:frame%
	    (init-field do-on-close)
	    (inherit show)
	    (public*
	      [get-kind (lambda () "Panel")]
	      [on-main (lambda (x) x)]
	      [find-control (lambda (tag) #f)])
	    (super-make-object (format "~a Settings" (get-kind)) #f 200 10)
	    (private-field
	      [main (on-main (make-object mred:vertical-panel% this))]
	      [name-edit (make-one-line/callback-edit main "Scheme Name:"
						      (lambda (txt)
							(set! name txt))
						      name)])
	    (field
	      [controls (make-object mred:vertical-panel% main)])
	    (augment*
	     [on-close (lambda () (do-on-close))])
	    (send controls set-alignment 'left 'center)
	    (let* ([p (make-object mred:vertical-panel% main)]
		   [make-sc
		    (lambda (name set)
		      (make-object mred:check-box% 
				   name
				   p
				   (lambda (c e)
				     (set (send c get-value))
				     (gb-need-recalc-size))))]
		   [xsc (make-sc "Allow Horizontal Stretching"
				 (lambda (on?) (set! x-stretch? on?)))]
		   [ysc (make-sc "Allow Vertical Stretching"
				 (lambda (on?) (set! y-stretch? on?)))])
	      (send p set-alignment 'left 'center)
	      (send xsc set-value x-stretch?)
	      (send ysc set-value y-stretch?)
	      (let ([p (make-object mred:vertical-panel% p)])
		(send p stretchable-height #f))))))
       
       (gb-add-child 
	(case-lambda 
	 [(c) (gb-add-child c (length children))]
	 [(c pos)
	  (set! children 
		(let loop ([l children][p pos])
		  (cond
		   [(or (zero? p) (null? l)) (cons c l)]
		   [else (cons (car l) (loop (cdr l) (sub1 p)))]))) 
	  (when pb
	    (send c gb-install pb this)
	    (send pb insert c x (+ y h)))
	  (gb-need-recalc-size)]))
       (gb-remove-child 
	(lambda (c) 
	  (set! children (remq c children))
	  (gb-need-recalc-size)))
       
       (gb-need-recalc-size
	(lambda ()
	  (set! need-recalc? #t)
	  (resized)))
       
       (gb-install
	(lambda (pb-in parent-in)
	  (set! parent parent-in)
	  (if pb
	      (when parent
		(send pb set-before this parent))
	      (set! pb pb-in))
	  (set! id (send pb new-id))
	  (for-each
	   (lambda (c)
	     (send pb insert c x (+ y h))
	     (send c gb-install pb this))
	   children)))
       
       (gb-get-child-x-start
	(lambda (mw mh w h) 
	  0))
       (gb-get-child-y-start
	(lambda (mw mh w h)
	  (if (or (= vertical-child-alignment 1)
		  (ormap (lambda (c) (gb-y-stretch? c)) children))
	      0
	      (case vertical-child-alignment
		[(2) (/ (- h mh) 2)]
		[(3) (- h mh)]))))
       (gb-combine-child-width (lambda (a b) (max a b)))
       (gb-combine-child-height (lambda (a b) (spacing-+ a b)))
       
       (gb-compute-child-x-pos
	(lambda (dc c w)
	  (if (gb-x-stretch? c)
	      0
	      (case horizontal-child-alignment
		[(2) (let-values ([(cw ch) (send c gb-get-min-size dc)])
		       (/ (- w cw) 2))]
		[(1) 0]
		[(3) (let-values ([(cw ch) (send c gb-get-min-size dc)])
		       (- w cw))]))))
       (gb-compute-child-y-pos
	(lambda (dc c h)
	  0))
       (gb-compute-child-width
	(lambda (dc c w xsc dw)
	  (if (gb-x-stretch? c)
	      w
	      (let-values ([(cw ch) (send c gb-get-min-size dc)])
		cw))))
       (gb-compute-child-height
	(lambda (dc c h ysc dh)
	  (let-values ([(cw ch) (send c gb-get-min-size dc)])
	    (if (gb-y-stretch? c)
		(+ ch (/ dh ysc))
		ch))))
       
       (gb-combine-child-x-offset (lambda (a b) a))
       (gb-combine-child-y-offset (lambda (a b) (spacing-+ a b)))
       
       (gb-get-min-size
	(lambda (dc)
	  (let loop ([lw 0][lh 0][l children])
	    (cond
	     [(null? l) (let* ([w (+ lw lm rm)]
			       [h (+ lh tm bm)])
			  (set! prev-min-h h)
			  (values w h))]
	     [else
	      (let ([c (car l)])
		(let-values ([(cw ch) (send c gb-get-min-size dc)])
		  (loop (gb-combine-child-width lw cw) 
			(gb-combine-child-height lh ch) 
			(cdr l))))]))))
       (gb-set-shape
	(lambda (dc x-in y-in w-in h-in)
	  (let*-values ([(xsc) (apply + (map 
					 (lambda (c) (if (gb-x-stretch? c) 1 0))
					 children))] 
			[(ysc) (apply + (map 
					 (lambda (c) (if (gb-y-stretch? c) 1 0))
					 children))]
			[(mw mh) (gb-get-min-size dc)]
			[(ew eh) (values (- w-in lm rm) (- h-in tm bm))]
			[(dw dh) (values (- w-in mw) (- h-in mh))])
	    (let loop ([lx (+ lm x-in (gb-get-child-x-start mw mh w-in h-in))]
		       [ly (+ tm y-in (gb-get-child-y-start mw mh w-in h-in))]
		       [l children])
	      (cond
	       [(null? l) 0]
	       [else
		(let ([c (car l)])
		  (let-values ([(cw ch)
				(send c gb-set-shape dc
				      (+ lx (gb-compute-child-x-pos dc c ew))
				      (+ ly (gb-compute-child-y-pos dc c eh))
				      (gb-compute-child-width dc c ew xsc dw)
				      (gb-compute-child-height dc c eh ysc dh))])
		    (loop (gb-combine-child-x-offset lx cw) 
			  (gb-combine-child-y-offset ly ch) 
			  (cdr l))))])))
	  (unless parent
	    (when (and pb (not (and (= w w-in) (= h h-in))))
	      (send pb top-resized this w h w-in h-in)))
	  (set! x x-in)
	  (set! y y-in)
	  (set! w w-in)
	  (set! h h-in)
	  (resized)
	  (when pb
	    (send pb move-to this x-in y-in))
	  (values w-in h-in)))
       
       (find-position-<
	(lambda (fx fy cx cy)
	  (< fy cy)))
       (gb-find-position
	(lambda (fx fy)
	  (let loop ([l children][pos 0])
	    (if (null? l)
		pos
		(let*-values ([(c) (car l)]
			      [(cx) (send c gb-get-stable-x)]
			      [(cy) (send c gb-get-stable-y)]
			      [(w h) (send c gb-get-size)])
		  (if (find-position-< fx fy (+ cx w) (+ cy h))
		      pos
		      (loop (cdr l) (add1 pos))))))))
       (gb-get-child-pos
	(lambda (c)
	  (let loop ([l children][pos 0])
	    (cond
	     [(null? l) pos]
	     [(eq? (car l) c) pos]
	     [else (loop (cdr l) (add1 pos))]))))
       
       (gb-get-saved-min-size
	(lambda ()
	  (values prev-min-w prev-min-h)))
       
       (gb-recalc-size
	(lambda (dc)
	  (if parent
	      (send parent gb-recalc-size dc)
	      (let-values ([(mw mh) (gb-get-min-size dc)]
			   [(xb) (box 0)]
			   [(yb) (box 0)])
		(when pb 
		  (send pb get-snip-location this xb yb #f)
		  (send pb get-main-location this dc xb yb))
		(gb-set-shape dc (unbox xb) (unbox yb) 
			      (if x-stretch? (max w mw) mw)
			      (if y-stretch? (max h mh) mh))))))
       
       (gb-hilite
	(lambda (on?)
	  (unless (eq? on? hilited?)
	    (set! hilited? on?)
	    (refresh))))
       
       (gb-get-parent
	(lambda () parent))
       (gb-get-children
	(lambda () children))
       (gb-get-size
	(lambda () (values w h)))
       (gb-get-x (lambda () x))
       (gb-get-y (lambda () y))
       (gb-get-stable-x (lambda () stable-x))
       (gb-get-stable-y (lambda () stable-y))
       (gb-get-position-and-size
	(lambda () (values x y w h)))
       
       (gb-set-stable-position
	(lambda ()
	  (set! stable-x x)
	  (set! stable-y y)))
       
       (gb-drag-children-along
	(lambda (new-x new-y)
	  (when (not (and (= x new-x) (= y new-y)))
	    (for-each
	     (lambda (c)
	       (let ([cx (+ new-x (- (send c gb-get-stable-x) stable-x))]
		     [cy (+ new-y (- (send c gb-get-stable-y) stable-y))])
		 (send pb move-to c cx cy)
		 (send c gb-drag-children-along cx cy)))
	     children)
	    (set! x new-x)
	    (set! y new-y))))
       
       (gb-open-dialog
	(lambda ()
	  (if dialog
	      (send dialog show #t)
	      (let ([f (make-object (get-frame%) (lambda () (set! dialog #f)))])
		(set! dialog f)
		(send f show #t)))))
       
       (gb-reconnect-to-original-children
	(lambda ()
	  (if original-children-ids
	      (let ([sl (map
			 (lambda (id) (send pb find-snip-by-original-id id))
			 original-children-ids)])
		(set! original-children-ids #f)
		(for-each
		 (lambda (s)
		   (when s
		     (gb-add-child s)
		     (send pb remove-selected s)))
		 sl)
		#t)
	      #f)))
       (gb-forget-original-id
	(lambda ()
	  ;; Make unique name
	  (let ([orig-name name])
	    (set! name #f)
	    (let loop ([new-name orig-name])
	      (if (send pb find-snip-by-name new-name)
		  (loop (string-append new-name "+"))
		  (set! name new-name))))
	  (set! original-id #f)
	  (set! original-children-ids #f)))
       
       (gb-get-instantiate-class-getter
	(lambda ()
	  `(,(string->symbol (string-append "get-" name "%")))))
       (gb-get-style
	(lambda ()
	  (if with-border?
	      '(border)
	      null)))
       (gb-local-instantiate
	(lambda (parent mode)
	  `(new ,(if (output-mode-as-class? mode)
		     (gb-get-instantiate-class-getter)
		     (gb-get-default-class))
		[parent ,parent]
		,@(gb-instantiate-arguments))))
       (gb-instantiate-arguments
	(lambda () `([style ',(gb-get-style)]
		     [stretchable-width ,x-stretch?]
		     [stretchable-height ,y-stretch?])))

       (gb-get-default-class (lambda () 'vertical-panel%))
       (gb-aux-instantiate
	(lambda (mode)
	  (if (output-mode-as-class? mode)
	      `((public* [,(string->symbol (string-append "get-" name "%"))
			  (lambda () ,(gb-get-default-class))]))
	      null)))
       (gb-instantiate
	(lambda (parent mode)
	  (let ([v (gb-local-instantiate parent mode)]
		[name (string->symbol name)])
	    `(,@(gb-aux-instantiate mode)
	      ,(if (output-mode-as-class? mode)
		   `(field [,name ,v])
		   `(define ,name ,v))
	      ,@(apply append
		       (map (lambda (c) (send c gb-instantiate name mode)) children))))))

       (draw-box
	(lambda (dc x y w h)
	  (let* ((xw (sub1 (+ x w)))
		 (yh (sub1 (+ y h)))
		 (x (add1 x))
		 (y (add1 y)))
	    (send dc draw-line x y xw y)
	    (send dc draw-line xw y xw yh)
	    (send dc draw-line x yh xw yh)
	    (send dc draw-line x y x yh))))
       
       (base-setup
	(lambda (nm xs? ys? nw nh hca vca wb? id children-ids)
	  (set! name nm)
	  (set! x-stretch? xs?)
	  (set! y-stretch? ys?)
	  (set! w nw)
	  (set! h nh)
	  (set! horizontal-child-alignment hca)
	  (set! vertical-child-alignment vca)
	  (set! with-border? wb?)
	  (set! original-id id)
	  (set! original-children-ids children-ids)))

       [get-tagged-value
	(lambda (tag) #f)]
       [set-tagged-value (lambda (t v-in) (void))]

	(refresh
	 (lambda ()
	   (let ([admin (get-admin)])
	     (when admin
	       (send admin needs-update this 0 0 w h)))))
	(resized
	 (lambda ()
	   (let ([admin (get-admin)])
	     (when admin
	       (send admin resized this #t))))))

      (override*
       (get-extent
	(lambda (dc x y wbox hbox descentbox spacebox
		    lspacebox rspacebox)
	  (when need-recalc?
	    (set! need-recalc? #f)
	    (gb-recalc-size dc))
	  (when hbox
	    (set-box! hbox h))
	  (when wbox
	    (set-box! wbox w))
	  (when descentbox
	    (set-box! descentbox 0))
	  (when spacebox
	    (set-box! spacebox 0))
	  (when rspacebox
	    (set-box! rspacebox 0))
	  (when lspacebox
	    (set-box! lspacebox 0))))
       (draw
	(lambda (dc x y . other)
	  (draw-box dc x y w h)
	  (when (or with-border? hilited?)
	    (draw-box dc (add1 x) (add1 y) (- w 2) (- h 2)))
	  (when (and with-border? hilited?)
	    (draw-box dc (+ 2 x) (+ 2 y) (- w 4) (- h 4)))))
       (copy
	(lambda ()
	  (let ([o (make-object (hash-table-get interface->class-table
						(object-interface this))
				lm tm rm bm)])
	    (send o base-setup 
		  name
		  x-stretch? y-stretch? w h 
		  horizontal-child-alignment
		  vertical-child-alignment
		  with-border?
		  (or original-id id)
		  (or original-children-ids 
		      (and (pair? children)
			   (map (lambda (child) (gb-id child)) children))))
	    o)))
       (write
	(lambda (stream)
	  (send stream put (string->bytes/utf-8 name))
	  (send stream put (if x-stretch? 1 0))
	  (send stream put (if y-stretch? 1 0))
	  (send stream put (floor (inexact->exact w)))
	  (send stream put (floor (inexact->exact h)))
	  (send stream put horizontal-child-alignment)
	  (send stream put vertical-child-alignment)
	  (send stream put (if with-border? 1 0))
	  (send stream put (string->bytes/utf-8 (if id id "BAD")))
	  (stream-write-list stream (map (lambda (c) (gb-id c)) children)))))
      (public*
       (read
	(lambda (stream version)
	  (base-setup
	   ;; name
	   (if (>= version 3) 
	       ((get-bytes->string version) (send stream get-bytes))
	       name)
	   (positive? (send stream get-exact))
	   (positive? (send stream get-exact))
	   (send stream get-exact) ; w
	   (send stream get-exact) ; h
	   (if (>= version 2) (send stream get-exact) horizontal-child-alignment) ; hca
	   (if (>= version 2) (send stream get-exact) vertical-child-alignment) ; vca
	   (if (>= version 2) (positive? (send stream get-exact)) #f) ; with-border?
	   ((get-bytes->string version) (send stream get-bytes))
	   (let ([v (stream-read-list stream version)])
	     (if (null? v) #f v))))))
      (override*
       (resize 
	(lambda (w-in h-in)
	  (if (not parent) 
	      (let-values ([(mw mh) (values prev-min-w prev-min-h)])
		(if (or (and (> w-in mw) x-stretch?)
			(and (> h-in mh) y-stretch?))
		    (begin
		      (when x-stretch? (set! w (max mw w-in)))
		      (when y-stretch? (set! h (max mh h-in)))
		      (gb-need-recalc-size)
		      #t)
		    #f))
	      #f))))
      (super-new)
      (set-snipclass (send (mred:get-the-snip-class-list) find (get-classname)))
      (set-count 1)))

  (define gb:atomic-snip%
    (class gb:snip%
      (override*
       (init-x-stretch? (lambda () #f))
       (init-y-stretch? (lambda () #f))
       (container? (lambda () #f)))
      (super-new)))
  
  (define gb-y-stretch? (class-field-accessor gb:snip% y-stretch?))
  (define gb-x-stretch? (class-field-accessor gb:snip% x-stretch?))
  (define gb-id (class-field-accessor gb:snip% id))
  (define gb-original-id (class-field-accessor gb:snip% original-id))
  (define gb-parent (class-field-accessor gb:snip% parent))
  (define gb-name (class-field-accessor gb:snip% name))
  
  (define interface->class-table (make-hash-table))

  (define register-class
    (lambda (class% classname)
      (hash-table-put!
       interface->class-table
       (class->interface class%)
       class%)
      (let ([snipclass
	     (make-object 
	      (class mred:snip-class% ()
		(inherit set-classname set-version reading-version)
		(override*
		  [read
		   (lambda (stream)
		     (let ([o (make-object class%)])
		       (send o read stream (reading-version stream))
		       o))])
		(super-new)
		(set-classname classname)
		(set-version GB:SNIP-VERSION)))])
	(send (mred:get-the-snip-class-list) add snipclass))))
  
  (register-class gb:snip% "gb:core")
  
  (provide gb:snip%
	   gb:atomic-snip%

	   gb-y-stretch?
	   gb-x-stretch?
	   gb-id
	   gb-original-id
	   gb-parent
	   gb-name

	   (struct output-mode (as-class? no-free-vars?))
	   
	   interface->class-table

	   register-class))
