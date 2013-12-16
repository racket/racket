(module helper mzscheme
  (require mzlib/class
           (prefix wx: "kernel.rkt")
           (prefix wx: racket/snip/private/style)
           "lock.rkt")

  (provide (protect (struct child-info (x-min y-min x-margin y-margin x-stretch y-stretch))
                    get-two-int-values
                    non-negative-number?
                    same-dimension?
                    list-diff
                    key-regexp
                    do-command
                    double-boxed
                    queue-window-callback
                    param
                    protect&
                    find-pos
                    no-stretch
                    font->delta
                    traverse
                    object->position
                    container->children
                    filter-overlapping
                    system-position-ok-before-cancel?
                    ok-cancel))

  ;; this structure holds the information that a child will need to send
  ;; to its parent when the parent must resize itself.
  (define-struct child-info (x-min y-min           ; includes margins!
				   x-margin y-margin     ; requested margin space
				   x-stretch y-stretch)) ; booleans indicating strechability

  ;; get-two-int-values: a wrapper around functions that need to return
  ;;   two results.
  ;; input: function: a function which takes two boxes and returns results
  ;;          in them.
  ;; returns: the contents of the two boxes (as multiple values)
  (define get-two-int-values
    (lambda (function)
      (let ([a (box 0)]
	    [b (box 0)])
	(function a b)
	(values (unbox a) (unbox b)))))

  (define non-negative-number?
    (lambda (n)
      (and (real? n) (not (negative? n)))))

  (define same-dimension?
    (lambda (new-dim current-dim)
      (or (= new-dim current-dim)
	  (= new-dim -1))))
  
  ;; list-diff: computes the difference between two lists
  ;; input: l1, l2: two lists
  ;; returns:  a list of all elements in l1 which are not in l2.
  (define list-diff
    (lambda (l1 l2)
      (let ([table (make-hash-table)])
	(for-each
	 (lambda (item)
	   (hash-table-put! table item #t))
	 l2)
	(let loop ([l l1])
	  (cond
	   [(null? l) null]
	   [(hash-table-get table (car l) (lambda () #f))
	    (loop (cdr l))]
	   [else (cons (car l) (loop (cdr l)))])))))

  (define (key-regexp c)
    (regexp (format "(^|[^&])&[~a~a]" (char-downcase c) (char-upcase c))))
  
  (define (do-command c e)
    (as-exit (lambda () (send c command e))))

  (define double-boxed
    (lambda (x y f)
      (let ([x (box x)][y (box y)])
	(f x y)
	(values (unbox x) (unbox y)))))

  (define (queue-window-callback w cb)
    (parameterize ([wx:current-eventspace (send (send w get-top-level) get-eventspace)])
      (wx:queue-callback cb wx:middle-queue-key)))

  (define-syntax (param stx)
    (syntax-case stx ()
      [(_ get-obj method)
       (syntax/loc stx
	 (entry-point
	  (case-lambda
	   [() (send (get-obj) method)]
	   [(v) (send (get-obj) method v)])))]))

  (define (font->delta f)
    (define d (make-object wx:style-delta%))
    (let ([v (send f get-face)]
	  [m (send f get-family)])
      (if v
	  (send d set-delta-face v m)
	  (send d set-delta 'change-family m)))
    (send d set-delta 'change-size (send f get-point-size))
    (send d set-delta 'change-style (send f get-style))
    (send d set-delta 'change-weight (send f get-weight))
    (send d set-delta 'change-underline (send f get-underlined))
    (send d set-delta 'change-smoothing (send f get-smoothing))
    (send d set-delta 'change-size-in-pixels (send f get-size-in-pixels))
    d)

  (define protect&
    (lambda (s)
      (regexp-replace* #rx"&" s "\\&\\&")))

  (define (find-pos l i eq?)
    (let loop ([l l][n 0])
      (cond
       [(null? l) #f]
       [(eq? (car l) i) n]
       [else (loop (cdr l) (add1 n))])))

  (define (no-stretch a) 
    (send a stretchable-width #f) (send a stretchable-height #f))
  
  ;; ;;;;;;;;;;;;; Focus-tabbing helpers ;;;;;;;;;;;;;;;;;;;;

  (define (traverse x y w h dir dests)
    ;; x, y : real = starting positions
    ;; dir : one of 'left, 'right, 'up, 'next, 'prev = desried move
    ;; dests : list of (cons key x y w h) = destinations
    ;; returns key or #f
    (case dir
      [(next prev)
       (letrec ([get-x cadr]
		[get-w cadddr]
		[get-y caddr]
		[get-h (lambda (x) (caddr (cddr x)))]
		[backward? (eq? dir 'prev)]
		[fail-start (if backward?
				1000000000
				0)]
		[find-stripe (lambda (t stripes)
			       (let loop ([s stripes])
				 (cond
				  [(null? s) #f]
				  [(and (<= (caar s) t) (< t (cdar s)))
				   (car s)]
				  [else (loop (cdr s))])))]
		[mk-stripes
		 (lambda (get-y get-h stripes dests)
		   (let loop ([l (append (map (lambda (x) (cons (car x) (- (cdr x) (car x))))
					      stripes)
					 (map (lambda (x) 
						(cons (get-y x) (get-h x)))
					      dests))])
		     (if (null? l)
			 null
			 ;; Find longest top-most
			 (let* ([top (let loop ([l (cdr l)][best (car l)])
				       (cond
					[(null? l) best]
					[(or (< (caar l) (car best)) ; topmost
					     (and (= (caar l) (car best)) ; at least as top
						  (> (cdar l) (cdr best)))) ; longer
					 (loop (cdr l) (car l))]
					[else (loop (cdr l) best)]))]
				[t (car top)]
				[b (+ t (cdr top))])
			   ;; Stripe is anything that starts before the end of `top'
			   (let ([remaining (let loop ([l l])
					      (cond
					       [(null? l) null]
					       [(find-stripe (caar l) (list (cons t b)))
						(loop (cdr l))]
					       [else (cons (car l) (loop (cdr l)))]))])
			     (cons (cons t b) (loop remaining)))))))]
		[in-stripe (lambda (stripe dests get-y get-h)
			     (let loop ([l dests])
			       (cond
				[(null? l) null]
				[(find-stripe (get-y (car l)) (list stripe))
				 (cons (car l) (loop (cdr l)))]
				[else (loop (cdr l))])))]
		[next-stripe (lambda (stripe stripes)
			       (let loop ([s stripes][best #f])
				 (cond
				  [(null? s) best]
				  [(and (or (not stripe)
					    (if backward?
						(<= (cdar s) (car stripe))
						(>= (caar s) (cdr stripe))))
					(or (not best)
					    (if backward?
						(> (cdar s) (cdr best))
						(< (caar s) (cdr best)))))
				   (loop (cdr s) (car s))]
				  [else (loop (cdr s) best)])))]
		[find (lambda (v? get-x get-w get-y get-h use-x? x w use-y? y h dests fail)
			;; find's variable names correspond to an h-stripe view, but everything is
			;;  flipped to v-stripes if the args are flipped
			(let ([h-stripes (mk-stripes get-y get-h 
						     (if use-y? (list (cons y (+ y h))) null)
						     dests)])

			  ;; find the initial h-stripe
			  (let sel-h-stripe-loop ([init-h-stripe (if use-y?
								     (find-stripe y h-stripes)
								     (next-stripe #f h-stripes))]
						  [x x][w w][use-x? use-x?])
			    
			    ;; find items in the initial stripe
			    (let ([in-init-h-stripe (in-stripe init-h-stripe dests get-y get-h)]
				  [next (lambda ()
					  (let ([s (next-stripe init-h-stripe h-stripes)])
					    (if s
						(sel-h-stripe-loop s fail-start fail-start #f)
						(fail))))])

			      (if (null? in-init-h-stripe)

				  ;; no items in this stripe; try the next one
				  (next)
				  
				  ;; Non-empty h-stripe; now look for items in the same or later v-stripe
				  (if (or (null? (cdr in-init-h-stripe))
                                          ;; If we're already in "v-stripe" mode, then flipping back
                                          ;; to h-stripe mode is going to loop forever, so treat the
                                          ;; current strip as having only one item. (This should
                                          ;; happen only if the start positions overlap with the
                                          ;; destination positions.)
                                          v?)
				      
				      ;; one item in the stripe; take it unless we're using x and it's
				      ;;  before x:
				      (if (or (not use-x?)
					      ((if backward? < >) (get-x (car in-init-h-stripe)) x))
					  (car in-init-h-stripe)
					  
					  ;; Only item is no good; try the next stripe
					  (next))
				      
				      ;; Recur to work with v-stripes
				      (find #t get-y get-h get-x get-w use-y? y h use-x? x w in-init-h-stripe next)))))))])
	 (if (null? dests)
	     #f
	     (car (find #f get-x get-w get-y get-h #t x w #t y h dests
			(lambda ()
			  (find #f get-x get-w get-y get-h 
				#f fail-start fail-start 
				#f fail-start fail-start 
				dests void))))))]
      [else
       (let ([v (let loop ([d dests])
		  (if (null? d)
		      #f
		      (let* ([best (loop (cdr d))]
			     [this (car d)]
			     [diff (lambda (v l x w)
				     (cond
				      [(< (+ v l) x) (- x (+ v l))]
				      [(< (+ x w) v) (- (+ x w) v)]
				      [else 0]))])
			(let* ([get-x cadr]
			       [get-w cadddr]
			       [get-y caddr]
			       [get-h (lambda (x) (caddr (cddr x)))]
			       [tdx (diff x w (get-x this) (get-w this))]
			       [tdy (diff y h (get-y this) (get-h this))]
			       [bdx (and best (diff x w (get-x best) (get-w best)))]
			       [bdy (and best (diff y h (get-y best) (get-h best)))]
			       [better (lambda (tdx tdy bdy negative?)
					 (if (and (zero? tdx) (negative? tdy)
						  (or (not best) 
						      (< (abs tdy) (abs bdy))))
					     this
					     best))])
			  (case dir
			    [(up) (better tdx tdy bdy negative?)]
			    [(down) (better tdx tdy bdy positive?)]
			    [(left) (better tdy tdx bdx negative?)]
			    [(right) (better tdy tdx bdx positive?)])))))])
	 (and v (car v)))]))

  (define (object->position o)
    (let-values ([(x y) (double-boxed 0 0 (lambda (x y) (send o client-to-screen x y)))]
		 [(w h) (double-boxed 0 0 (lambda (x y) (send o get-client-size x y)))])
      (send o tabbing-position x y w h)))

  (define (container->children f except must-focus?)
    (apply
     append
     (map
      (lambda (i)
        (append
         (if (and (send i has-tabbing-children?)
                  (send i is-shown-to-root?))
             (container->children i except must-focus?)
             null)
         (cond
          [(or (eq? i except) 
               (and must-focus? (not (send i gets-focus?)))
               (not (send i is-enabled-to-root?))
               (not (send i is-shown-to-root?)))
           null]
          [else (list i)])))
      (send f get-children))))

  (define (filter-overlapping l)
    (if (null? l)
	null
	(let* ([rest (filter-overlapping (cdr l))]
	       [first (car l)]
	       [f (cdr first)]
	       [x (car f)]
	       [y (cadr f)]
	       [x2 (+ x (caddr f))]
	       [y2 (+ y (cadddr f))])
	  (if (ormap (lambda (other)
		       (let* ([p (cdr other)]
			      [px (car p)]
			      [py (cadr p)]
			      [px2 (+ px (caddr p))]
			      [py2 (+ py (cadddr p))])
			 (and (or (<= x px x2) (<= x px2 x2)
				  (<= px x px2) (<= px x2 px2))
			      (or (<= y py y2) (<= y py2 y2)
				  (<= py y py2) (<= py y2 py2)))))
		     rest)
	      rest
	      (cons first rest)))))

  (define (system-position-ok-before-cancel?)
    (eq? (system-type) 'windows))
  
  (define (ok-cancel mk-ok mk-cancel)
    (if (system-position-ok-before-cancel?)
        (values (mk-ok) (mk-cancel))
        (let ([c (mk-cancel)]
              [o (mk-ok)])
          (values o c)))))
