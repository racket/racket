(module ft-qq "mzscheme-core.ss"
  (require (as-is:unchecked mzscheme define-values define-syntaxes require-for-syntax
  raise-type-error quote unquote unquote-splicing))
  ;(require-for-syntax frtime/frp)
  (require-for-syntax syntax/stx)
  
  
  (define-values (frp:qq-append)
    (lambda (a b)
      (if (list? a)
	  (append a b)
	  (raise-type-error 'unquote-splicing "proper list" a))))

  (define-syntaxes (frp:quasiquote)
    (let ([here (quote-syntax here)] ; id with module bindings, but not lexical
	  [unquote-stx (quote-syntax unquote)]
	  [unquote-splicing-stx (quote-syntax unquote-splicing)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form))
	(let-values
	    (((form) (if (stx-pair? (stx-cdr in-form))
			 (if (stx-null? (stx-cdr (stx-cdr in-form)))
			     (stx-car (stx-cdr in-form))
			     (raise-syntax-error #f "bad syntax" in-form))
			 (raise-syntax-error #f "bad syntax" in-form)))
	     ((normal)
	      (lambda (x old)
		(if (eq? x old)
		    (if (stx-null? x)
			(quote-syntax ())
			(list (quote-syntax quote) x))
		    x)))
	     ((apply-cons)
	      (lambda (a d)
		(if (stx-null? d)
		    (list (quote-syntax list) a)
		    (if (if (pair? d)
			    (module-identifier=? (quote-syntax list) (car d))
			    #f)
			(list* (quote-syntax list) a (cdr d))
			(list (quote-syntax cons) a d))))))
	  (datum->syntax-object
	   here
	   (normal
	    (letrec-values
		(((qq)
		  (lambda (x level)
		    (let-values
			(((qq-list)
			  (lambda (x level)
			    (let-values
				(((old-first) (stx-car x)))
			      (let-values
				  (((old-second) (stx-cdr x)))
				(let-values
				    (((first) (qq old-first level)))
				  (let-values
				      (((second) (qq old-second level)))
				    (let-values
					()
				      (if (if (eq? first old-first)
					      (eq? second old-second)
					      #f)
					  x
					  (apply-cons
					   (normal first old-first)
					   (normal second old-second)))))))))))
		      (if (stx-pair? x)
			  (let-values
			      (((first) (stx-car x)))
			    (if (if (if (identifier? first)
					(module-identifier=? first unquote-stx)
					#f)
				    (stx-list? x)
				    #f)
				(let-values
				    (((rest) (stx-cdr x)))
				  (if (let-values
					  (((g35) (not (stx-pair? rest))))
					(if g35 g35 (not (stx-null? (stx-cdr rest)))))
				      (raise-syntax-error
				       'unquote
				       "expects exactly one expression"
				       in-form
				       x))
				  (if (zero? level)
				      (stx-car rest)
				      (qq-list x (sub1 level))))
				(if (if (if (identifier? first)
					    (module-identifier=? first (quote-syntax frp:quasiquote))
					    #f)
					(stx-list? x)
					#f)
				    (qq-list x (add1 level))
				    (if (if (if (identifier? first)
						(module-identifier=? first unquote-splicing-stx)
						#f)
					    (stx-list? x)
					    #f)
					(raise-syntax-error
					 'unquote-splicing
					 "invalid context within quasiquote"
					 in-form
					 x)
					(if (if (stx-pair? first)
						(if (identifier? (stx-car first))
						    (if (module-identifier=? (stx-car first)
									     unquote-splicing-stx)
							(stx-list? first)
							#F)
						    #f)
						#f)
					    (let-values
						(((rest) (stx-cdr first)))
					      (if (let-values
						      (((g34) (not (stx-pair? rest))))
						    (if g34
							g34
							(not (stx-null? (stx-cdr rest)))))
						  (raise-syntax-error
						   'unquote
						   "expects exactly one expression"
						   in-form
						   x))
					      (let-values
						  (((uqsd) (stx-car rest))
						   ((old-l) (stx-cdr x))
						   ((l) (qq (stx-cdr x) level)))
						(if (zero? level)
						    (let-values
							(((l) (normal l old-l)))
						      (let-values
							  ()
							(list (quote-syntax frp:qq-append) uqsd l)))
						    (let-values
							(((restx) (qq-list rest (sub1 level))))
						      (let-values
							  ()
							(if (if (eq? l old-l)
								(eq? restx rest)
								#f)
							    x
							    (apply-cons
							     (apply-cons
							      (quote-syntax (quote unquote-splicing))
							      (normal restx rest))
							     (normal l old-l))))))))
					    (qq-list x level))))))
			  (if (if (syntax? x) 
				  (vector? (syntax-e x))
				  #f)
			      (let-values
				  (((l) (vector->list (syntax-e x))))
				(let-values
				    (((l2) (qq l level)))
				  (let-values
				      ()
				    (if (eq? l l2)
					x
					(list (quote-syntax list->vector) l2)))))
			      (if (if (syntax? x) (box? (syntax-e x)) #f)
				  (let-values
				      (((v) (unbox (syntax-e x))))
				    (let-values
					(((qv) (qq v level)))
				      (let-values
					  ()
					(if (eq? v qv)
					    x
					    (list (quote-syntax box) qv)))))
				  x)))))))
	      (qq form 0))
	    form)
	   in-form)))))
  
  (provide ;(rename frp:qq-append qq-append)
           (rename frp:quasiquote quasiquote)))
