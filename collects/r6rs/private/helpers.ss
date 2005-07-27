
(module helpers mzscheme
  (require (lib "list.ss")
	   "uri.ss")
  (require-for-template mzscheme)

  (provide translate-import
	   translate-export
	   uri?)

  (define (uri? s)
    ;; Need a proper test here!
    #t)

  (define ((check-identifier stx) id)
    (unless (identifier? id)
      (raise-syntax-error
       #f
       "expected an identifier"
       stx
       id)))

  (define (check-present orig-i what nested !not exceptions names)
    (for-each (lambda (en)
		(unless (!not (ormap (lambda (i) (bound-identifier=? (car en) i))
				    names))
		  (raise-syntax-error
		   #f
		   (format "~a in nested ~a" what nested)
		   orig-i
		   (car en))))
	      exceptions))

  (define (add-prefix prefix id)
    (if prefix
	(datum->syntax-object id
			      (string->symbol
			       (format "~a~a" (syntax-e prefix) (syntax-e id)))
			      id)
	id))

  (define (locate-rename id renames)
    (cond
     [(null? renames) #f]
     [(bound-identifier=? id (caar renames)) (cdar renames)]
     [else (locate-rename id (cdr renames))]))

  (define (apply-rename new-names old-names name-pairs rcons)
    (map (lambda (i)
	   (or (ormap (lambda (new old)
			(and (bound-identifier=? (car i) new)
			     (rcons old (cdr i))))
		      new-names old-names)
	       i))
	 name-pairs))

  (define (remove-all-prefixes orig-i name-pairs form prefix)
    (let ([s (symbol->string (syntax-e prefix))])
      (map (lambda (i)
	     (let ([old (symbol->string (syntax-e (car i)))])
	       (unless (and ((string-length old) . >= . (string-length s))
			    (string=? s (substring old 0 (string-length s))))
		 (raise-syntax-error
		  #f
		  (format "~a does not have prefix ~s added by nested `prefix' form"
			  form
			  s)
		  orig-i
		  (car i)))
	       (cons (datum->syntax-object (car i)
					   (string->symbol (substring old (string-length s)))
					   (car i))
		     (cdr i))))
	   name-pairs)))		  

  (define (check-unique-names orig-i what names)
    (let ([dup (check-duplicate-identifier names)])
      (when dup
	(raise-syntax-error
	 #f
	 (format "duplicate ~a identifier" what)
	 orig-i
	 dup))))

  (define (localize i stx)
    (datum->syntax-object i (syntax-e stx)))

  (define (translate-import i)
    (define orig-i #`(import #,i))
    (syntax-case* i (for run expand) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
      [(for sub run expand)
       (finish-translate-import orig-i #'sub #t #t)]
      [(for sub expand run)
       (finish-translate-import orig-i #'sub #t #t)]
      [(for sub run)
       (finish-translate-import orig-i #'sub #t #f)]
      [(for sub expand)
       (finish-translate-import orig-i #'sub #f #t)]
      [(for . _)
       (raise-syntax-error
	#f
	"bad `for' form"
	orig-i
	i)]
      [_else
       (finish-translate-import orig-i i #t #f)]))

  (define (finish-translate-import orig-i i run? expand?)
    (define (mk-require l)
      (cond
       [(and run? expand?)
	#`(begin (require #,@l) (require-for-syntax #,@l))]
       [run?
	#`(require #,@l)]
       [expand?
	#`(require-for-syntax #,@l)]))
    (translate-impexp
     i orig-i
     (lambda (i exceptions onlys renames extra-prefix)
       ;; Found a base URI?
       (unless (and (string? (syntax-e i))
		    (uri? (syntax-e i)))
	 (raise-syntax-error
	  #f
	  "expected a URI string or an `only', `except', `add-prefix', or `rename' form"
	  orig-i
	  i))
       (let ([name (datum->syntax-object i (uri->module-path (syntax-e i)) i)])
	 (cond
	  [onlys
	   ;; Onlys are implemented with `rename':
	   (mk-require (map (lambda (name-pair)
			      #`(rename #,name #,(cdr name-pair) #,(car name-pair)))
			    onlys))]
	  [(or exceptions (pair? renames))
	   ;; First import non-renamed, then renamed:
	   (mk-require (cons
			(localize i #`(#,(if extra-prefix #'prefix-all-except #'all-except)
				       #,@(if extra-prefix (list extra-prefix) null)
				       #,name
				       #,@(append (map car (or exceptions null))
						  (map car renames))))
			(map (lambda (i)
			       #`(rename #,name #,(cdr i) #,(car i)))
			     renames)))]
	  [extra-prefix
	   (mk-require (list (localize i #`(prefix #,extra-prefix #,name))))]
	  [else
	   (mk-require (list name))])))))

  (define (translate-export i)
    (define orig-i #`(export #,i))
    (translate-impexp 
     i orig-i
     (lambda (i exceptions onlys renames extra-prefix)
       (define (result l)
	 (when exceptions
	   (check-present orig-i "except not" "identifier" values exceptions l))
	 (when onlys
	   (check-present orig-i "only not" "identifier" values onlys l))
	 (when renames
	   (check-present orig-i "rename not" "identifier" values renames l))
	 (let* ([l (if exceptions
		       (filter (lambda (i)
				 (not (ormap (lambda (x) (bound-identifier=? (car x) i))
					     exceptions)))
			       l)
		       l)]
		[name-pairs (if onlys 
				onlys
				(apply-rename (map car renames) (map cdr renames)
					      (map cons l
						   (map (lambda (i) (add-prefix extra-prefix i)) l))
					      (lambda (b a) (cons a b))))])
	   (if (andmap (lambda (p) (eq? (car p) (cdr p)))
		       name-pairs)
	       #`(provide #,@(map car name-pairs))
	       #`(provide #,@(map (lambda (p)
				    #`(rename #,(car p) #,(cdr p)))
				  name-pairs)))))
       (syntax-case* i (set) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
	 [(set id ...)
	  (let ([ids (syntax->list #'(id ...))])
	    (for-each (lambda (x)
			(unless (identifier? x)
			  (raise-syntax-error
			   #f
			   "expected an identifier"
			   i
			   x)))
		      ids)
	    (result ids))]
	 [_else
	  (begin
	    (unless (identifier? i)
	      (raise-syntax-error
	       #f
	       "expected an identifier or a `set', `only', `except', `add-prefix', or `rename' form"
	       orig-i
	       i))
	    (result (list i)))]))))

  (define (translate-impexp i orig-i k)
    (let loop ([i i]
	       [exceptions #f]    ; #f if onlys
	       [onlys #f]         ; #f if exceptions
	       [renames null]     ; null if onlys
	       [extra-prefix #f]) ; #f if onlys, already folded into exceptions & renames
      (syntax-case* i (only except rename add-prefix) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
	[(only sub name ...)
	 (let ([names (syntax->list #'(name ...))])
	   (for-each (check-identifier i) names)
	   (check-unique-names orig-i "`only'" names)
	   (check-present orig-i "rename not" "`only' list" values renames names)
	   (cond
	    [exceptions
	     (check-present orig-i "except not" "`only' list" values exceptions names)
	     (loop #'sub
		   #f
		   (remove* exceptions (map (lambda (i) 
					      (cons i (or (locate-rename i renames)
							  (add-prefix extra-prefix i))))
					    names)
			    (lambda (a b) (bound-identifier=? (car a) (car b))))
		   null
		   #f)]
	    [onlys
	     (check-present orig-i "only not" "`only' list" values onlys names)
	     (loop #'sub #f onlys null #f)]
	    [else
	     (loop #'sub
		   #f
		   (map (lambda (i)
			  (cons i (or (locate-rename i renames)
				      (add-prefix extra-prefix i))))
			names)
		   null
		   #f)]))]
	[(only . _)
	 (raise-syntax-error
	  #f
	  "bad syntax"
	  i)]
	[(except sub name ...)
	 (let ([names (syntax->list #'(name ...))])
	   (for-each (check-identifier i) names)
	   (check-unique-names orig-i "`except'" names)
	   (check-present orig-i "rename" "`except' list" not renames names)
	   (let ([remove-exceptions
		  (lambda ()
		    (remove* exceptions renames (lambda (a b) (bound-identifier=? (car a) (car b)))))])
	     (cond
	      [(pair? exceptions)
	       (check-present orig-i "except" "`except' list" not exceptions names)
	       ;; union the exceptions
	       (loop #'sub
		     (append
		      (remove* exceptions (map (lambda (i) 
						 (cons i (or (locate-rename i renames)
							     (add-prefix extra-prefix i))))
					       names)
			       (lambda (a b) (bound-identifier=? (car a) (car b))))
		      exceptions)
		     #f
		     (remove-exceptions)
		     extra-prefix)]
	      [(pair? onlys)
	       (check-present orig-i "only" "`except' list" not onlys names)
	       (loop #'sub #f onlys null #f)]
	      [else
	       (loop #'sub
		     (map (lambda (i)
			    (cons i (add-prefix extra-prefix i)))
			  names) 
		     #f
		     (remove-exceptions)
		     extra-prefix)])))]
	[(except . _)
	 (raise-syntax-error
	  #f
	  "bad syntax"
	  i)]
	[(rename sub (new old) ...)
	 (let* ([new-names (syntax->list #'(new ...))]
		[old-names (syntax->list #'(old ...))]
		[name-pairs (map (lambda (old new)
				   (cons old (add-prefix extra-prefix new)))
				 old-names new-names)])
	   (for-each (check-identifier i) (apply append (map list new-names old-names)))
	   (check-unique-names orig-i "`rename' target" new-names)
	   (check-unique-names orig-i "`rename' source" old-names)
	   (let ([combine-renames
		  (lambda ()
		    (let ([renames (apply-rename new-names old-names renames cons)])
		      (append
		       renames
		       (remove* renames name-pairs 
				(lambda (a b)
				  (bound-identifier=? (car a) (car b)))))))])
	     (cond
	      [exceptions
	       (loop #'sub
		     (apply-rename new-names old-names exceptions cons)
		     #f
		     (combine-renames)
		     extra-prefix)]
	      [onlys
	       (loop #'sub
		     #f
		     (apply-rename new-names old-names onlys cons)
		     null
		     #f)]
	      [else
	       (loop #'sub
		     #f
		     #f
		     (combine-renames)
		     extra-prefix)])))]
	[(rename . _)
	 (raise-syntax-error
	  #f
	  "bad syntax"
	  i)]
	[(add-prefix sub prefix)
	 (cond
	  [onlys
	   (loop #'sub
		 #f
		 (remove-all-prefixes orig-i onlys "only" #'prefix)
		 null
		 #f)]
	  [else
	   (loop #'sub
		 (and exceptions
		      (remove-all-prefixes orig-i exceptions "except" #'prefix))
		 #f
		 (remove-all-prefixes orig-i renames "rename" #'prefix)
		 (add-prefix extra-prefix #'prefix))])]
	[(add-prefix . _)
	 (raise-syntax-error
	  #f
	  "bad syntax"
	  i)]
	[_else
	 (k i exceptions onlys renames extra-prefix)]))))
