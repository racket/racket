
(module multiple-choice mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "utils.ss"
	   "base.ss"
	   "feature.ss")
  
  (define gb:make-item-list-snip%
    (lambda (cl)
      (class cl
	(inherit gb-need-recalc-size)
	(private*
	  [delete
	   (lambda (l p)
	     (let loop ([l l][p p])
	       (cond
		 [(null? l) l]
		 [(zero? p) (cdr l)]
		 [else (cons (car l) (loop (cdr l) (sub1 p)))])))])
	(public*
	  [get-items
	   (lambda ()
	     items)]
	  [init-items (lambda () null)]
	  [get-item-height
	   (lambda (dc)
	     (let-values ([(w h d a) (send dc get-text-extent "Xj")])
	       h))]
	  [get-max-item-width
	   (lambda (dc)
	     (let loop ([l items][mw 0])
	       (if (null? l)
		   mw
		   (let-values ([(w h d a) (send dc get-text-extent (car l))])
		     (loop (cdr l) (max mw w))))))]
	  
	  [items-install
	   (lambda (l)
	     (set! items l))])
	(override*
	 [get-frame%
	  (lambda ()
	    (class (super get-frame%)
	      (inherit-field controls)
	      (super-new)
	      (public*
	       [user-item (lambda (v)
			    (mred:get-text-from-user "Item name:" "List Item Name" #f v))])
	      (private-field
		[items-panel (make-object mred:vertical-panel% controls)]
		[items-list (make-object mred:list-box%
					 "Items:" 
					 items
					 items-panel 
					 (lambda (l e)
					   (when (eq? 'list-box-dclick (send e get-event-type))
					     (let ([pos (send items-list get-selection)])
					       (unless (negative? pos)
						 (let ([v (user-item (list-ref items pos))])
						   (when v
						     (send items-list set-string pos v)
						     (set-car! (list-tail items pos) v)
						     (gb-need-recalc-size))))))))]
		[item-buttons-panel (let ([v (make-object mred:horizontal-panel% items-panel)])
				      (send v stretchable-width #f)
				      v)]
		[add-item (make-object mred:button% "Add Item" item-buttons-panel
				       (lambda (b e)
					 (let ([v (user-item (format 
							      "Item~a" 
							      (send items-list get-number)))])
					   (when v
					     (send items-list append v)
					     (set! items (append items (list v)))
					     (gb-need-recalc-size)))))]
		[delete-item (make-object mred:button% "Delete Item" item-buttons-panel
					  (lambda (b e)
					    (let loop ([ls (reverse (send items-list get-selections))])
					      (unless (null? ls)
						(send items-list delete (car ls))
						(set! items (delete items (car ls)))
						(loop (cdr ls))))
					    (gb-need-recalc-size)))])))]
	  [gb-instantiate-arguments
	   (lambda ()
	     (cons
	      `[choices ',(get-items)]
	      (super gb-instantiate-arguments)))]
	  [copy
	   (lambda ()
	     (let ([o (super copy)])
	       (send o items-install items)
	       o))]
	  [write
	   (lambda (stream)
	     (super write stream)
	     (stream-write-list stream items))]
	  [read
	   (lambda (stream version)
	     (super read stream version)
	     (items-install (stream-read-list stream version)))])
	(private-field
	  [items (init-items)])
	(super-new))))
   
  (define gb:make-list-box-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h)
	(inherit get-callback-names get-items get-item-height)
	(field
	  [min-body-width 50]
	  [sb-width 10]
	  [min-item-count 3])
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "listbox"))]
	  [init-y-stretch? (lambda () #t)]
	  [init-x-stretch? (lambda () #t)]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([y (get-item-height dc)])
	       (values min-body-width (* min-item-count y))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y w h)
	     (send dc draw-line 
		   (+ w x (- sb-width)) y
		   (+ w x (- sb-width)) (+ y h))
	     (with-clipping-region 
	      dc x y (- w sb-width) h
	      (lambda ()
		(let ([ih (get-item-height dc)])
		  (let loop ([l (get-items)][iy (add1 y)])
		    (unless (or (>= iy (+ y h)) (null? l))
		      (send dc draw-text (car l) (+ 2 x) iy)
		      (loop (cdr l) (+ iy ih))))))))]
	  [get-callback-kinds (lambda ()
				(list "-select-callback" "-double-select-callback"))]
	  [gb-get-default-class (lambda () 'list-box%)]
	  [gb-get-style
	   (lambda ()
	     (cons 'single
		   (super gb-get-style)))]
	  [gb-get-unified-callback
	   (lambda ()
	     (let-values ([(sel dbl) (apply values (get-callback-names))])
	       `(lambda (b e) 
		  (case (send e get-event-type)
		    [(list-box) (,sel b e)]
		    [(list-box-dclick) (,dbl b e)]))))])
	(super-new))))
  
  
  (define gb:list-box-snip% (gb:make-list-box-snip%
			     (gb:make-item-list-snip%
			      (gb:make-callback-snip%
			       (gb:make-text-labelled-snip% gb:atomic-snip% 
							    "List")))
			     "gb:listbox"))
  
  (register-class gb:list-box-snip% "gb:listbox")
  
  
  (define gb:make-radio-box-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h vertical-layout?)
	(inherit get-item-height get-max-item-width get-items
		 gb-need-recalc-size)
	(private-field
	  [circle-size 10]
	  [margin 2])
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "radiobox"))]
	  [init-items (lambda () (list "First" "Second"))]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([h (max (get-item-height dc) circle-size)]
		   [w (get-max-item-width dc)]
		   [l (length (get-items))])
	       (let-values ([(x-l y-l) (if vertical-layout?
					   (values 1 l)
					   (values l 1))])
		 (values (* (+ circle-size margin w) x-l)
			 (* h y-l)))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (let ([ih (max (get-item-height dc) circle-size)]
		   [iw (+ (get-max-item-width dc) circle-size margin)])
	       (let loop ([l (get-items)][iy y][ix x])
		 (unless (null? l)
		   (send dc draw-ellipse ix (+ iy (/ (- ih circle-size) 2)) circle-size circle-size)
		   (send dc draw-text (car l) (+ circle-size margin ix) iy)
		   (if vertical-layout?
		       (loop (cdr l) (+ iy ih) ix)
		       (loop (cdr l) iy (+ ix iw)))))))]
	  [gb-get-default-class (lambda () 'radio-box%)])
	(super-new))))
  
  
  (define gb:radio-box-snip% (gb:make-radio-box-snip%
			      (gb:make-item-list-snip%
			       (gb:make-layout-snip%
				(gb:make-callback-snip%
				 (gb:make-text-labelled-snip% gb:atomic-snip% 
							      "Radiobox"))))
			      "gb:radiobox"))
  
  (register-class gb:radio-box-snip% "gb:radiobox")
  
  
  (define gb:make-choice-snip%
    (lambda (cl cn)
      (class cl
	(inherit-field w h)
	(inherit get-item-height get-max-item-width get-items
		 gb-need-recalc-size)
	(field
	  [arrow-size 10]
	  [lmargin 2]
	  [amargin 2]
	  [rmargin 2]
	  [arrow (list (make-object mred:point% 0 0)
		       (make-object mred:point% arrow-size 0)
		       (make-object mred:point% (quotient arrow-size 2) (quotient arrow-size 2)))])
	(override*
	  [get-classname (lambda () cn)]
	  [init-name (lambda () (new-name "choice"))]
	  [init-items (lambda () (list "First"))]
	  [get-min-body-size
	   (lambda (dc)
	     (let ([h (get-item-height dc)]
		   [w (get-max-item-width dc)])
	       (values (+ lmargin arrow-size amargin w rmargin 3) (+ 3 h))))]
	  [draw-body
	   (lambda (dc x y w h)
	     (send dc draw-rectangle x y (sub1 w) (sub1 h))
	     (send dc draw-line (sub1 (+ x w)) (add1 y) (sub1 (+ x w)) (+ y h))
	     (send dc draw-line (add1 x) (sub1 (+ y h)) (+ x w) (sub1 (+ y h)))
	     (send dc draw-polygon arrow (+ 1 lmargin x) (+ y (/ (- h (/ arrow-size 2)) 2)))
	     (let ([l (get-items)])
	       (unless (null? l)
		 (send dc draw-text (car l) (+ 1 lmargin arrow-size amargin x) (add1 y)))))]
	  [gb-get-default-class (lambda () 'choice%)])
	(super-new))))
  
  
  (define gb:choice-snip% (gb:make-choice-snip%
			   (gb:make-item-list-snip%
			    (gb:make-callback-snip%
			     (gb:make-text-labelled-snip% gb:atomic-snip% 
							  "Choice")))
			   "gb:choice"))
  
  (register-class gb:choice-snip% "gb:choice")
  
  (provide gb:list-box-snip%
	   gb:radio-box-snip%
	   gb:choice-snip%))
