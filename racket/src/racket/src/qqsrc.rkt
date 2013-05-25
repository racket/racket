
(define qq
  (expand-defmacro
   '(#%define-macro -#%quasiquote
	(lambda (form)
	  (let ([normal
		 (lambda (x old)
		   (if (#%eq? x old)
		       (if (#%null? x)
			   x
			   (#%list '#%quote x))
		       x))])
	    (normal
	     (let qq ([x form][level 0])
	       (let ([qq-list
		      (lambda (x level)
			(let* ([old-first (#%car x)]
			       [old-second (#%cdr x)]
			       [first (qq old-first level)]
			       [second (qq old-second level)])
			  (if (and (#%eq? first old-first)
				   (#%eq? second old-second))
			      x
			      (#%list '#%cons 
				      (normal first old-first)
				      (normal second old-second)))))])
		 (#%cond
		  [(#%pair? x)
		   (let ([first (#%car x)])
		     (#%cond
		      [(and (#%eq? first 'unquote) (#%list? x))
		       (let ([rest (#%cdr x)])
			 (if (#%or (#%not (#%pair? rest))
				   (#%not (#%null? (#%cdr rest))))
			     (#%raise-syntax-error
			      'unquote
			       "takes exactly one expression"
			       (#%list 'quasiquote form)))
			 (if (#%zero? level)
			     (#%car rest)
			     (qq-list x (#%sub1 level))))]
		      [(and (#%eq? first 'quasiquote) (#%list? x))
		       (qq-list x (#%add1 level))]
		      [(and (#%eq? first 'unquote-splicing) (#%list? x))
		       (#%raise-syntax-error
			'unquote-splicing
			"invalid context within quasiquote"
			(#%list 'quasiquote form))]
		      [(and (#%pair? first)
			    (#%eq? (#%car first) 'unquote-splicing)
			    (#%list? first))
		       (let ([rest (#%cdr first)])
			 (if (or (#%not (#%pair? rest))
				 (#%not (#%null? (#%cdr rest))))
			     (#%raise-syntax-error
			      'unquote-splicing
			      "takes exactly one expression"
			      (#%list 'quasiquote form)))
			 (let ([uqsd (#%car rest)]
			       [old-l (#%cdr x)]
			       [l (qq (#%cdr x) level)])
			   (if (#%zero? level)
			       (let* ([l (normal l old-l)])
				 (#%list '#%append uqsd l))
			       (let* ([restx (qq-list rest (#%sub1 level))])
				 (if (and (#%eq? l old-l)
					  (#%eq? restx rest))
				     x
				     (#%list '#%cons 
					     (#%list '#%cons 
						     (#%list '#%quote 'unquote-splicing)
						     (normal restx rest))
					   (normal l old-l)))))))]
		      [else
		       (qq-list x level)]))]
		  [(#%vector? x)
		   (let* ([l (#%vector->list x)]
			  [l2 (qq l level)])
		     (if (#%eq? l l2)
			 x
			 (#%list '#%list->vector l2)))]
		  [(#%box? x)
		   (let* ([v (#%unbox x)]
			  [qv (qq v level)])
		     (if (#%eq? v qv)
			 x
			 (#%list '#%box qv)))]
		  [else x])))
	     form))))))

