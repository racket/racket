
(module awk mzscheme
  (require-for-syntax syntax/stx)

  (provide awk match:start match:end match:substring regexp-exec)

  (define-syntax awk
    (lambda (stx)
      (syntax-case stx ()
	[(_ next-record
	    (record field ...)
	    counter
	    ((state-variable init-expr) ...)
	    continue
	    clause ...)
	 (and (identifier? (syntax counter))
	      (identifier? (syntax continue)))
	 (let ([clauses (syntax->list (syntax (clause ...)))]
	       [initvars null])
	   (with-syntax ([(local-state ...) (generate-temporaries
					     (syntax->list (syntax (state-variable ...))))])
	     (letrec ([get-after-clauses
		       (lambda ()
			 (let loop ([l clauses][afters null])
			   (cond
			    [(null? l) (if (stx-null? afters)
					   (syntax ((values state-variable ...)))
					   afters)]
			    [(syntax-case (car l) (after)
			       [(after . rest) (syntax rest)]
			       [_else #f])
			     => (lambda (rest)
				  (with-syntax ([(after ...) afters])
				    (loop (cdr l) (syntax (after ... .  rest)))))]
			    [else
			     (loop (cdr l) afters)])))]
		      [wrap-state
		       (lambda (e)
			 (syntax-case e (=>)
			   [(=> f)
			    (with-syntax ([body (wrap-state (syntax ((f arg))))])
			      (syntax (=> (lambda (arg)
					    . body))))]
			   [body
			    (syntax
			     ((call-with-values (lambda () . body)
				(lambda (local-state ... . extras)
				  (set! else-ready? #f)
				  (set! state-variable local-state)
				  ...))))]))]
		      [make-range
		       (lambda (include-on? include-off? body rest)
			 (syntax-case body ()
			   [(t1 t2 . body)
			    (with-syntax ([on? (car (generate-temporaries '(1)))]
					  [t1 (make-test (syntax-e (syntax t1)) (syntax t1))]
					  [t2 (make-test (syntax-e (syntax t2)) (syntax t2))]
					  [body (wrap-state (syntax body))])
			      (with-syntax ([check (if include-on?
						       (if include-off?
							   (syntax post-on-on?)
							   (syntax on?))
						       (if include-off?
							   (syntax orig-on?)
							   (syntax (and orig-on? on?))))])
				(set! initvars (cons (syntax (on? #f)) initvars))
				(syntax
				 ((let ([orig-on? on?])
				    (unless on? 
				      (set! on? t1))
				    (let ([post-on-on? on?])
				      (when on? 
					(set! on? (not t2))))
				    (when check
				      . body))
				  . rest))))]
			   [_else (raise-syntax-error
				   #f
				   "bad range"
				   stx
				   body)]))]
		      [make-test
		       (lambda (test expr)
			 (cond
			  [(string? test)
			   (with-syntax ([g (car (generate-temporaries '(1)))]
					 [expr expr])
			     (set! initvars (cons (syntax (g (regexp expr))) initvars))
			     (syntax (regexp-exec g record)))]
			  [(number? test)
			   (with-syntax ([expr expr])
			     (syntax (= expr counter)))]
			  [else expr]))]
		      [get-testing-clauses
		       (lambda ()
			 (let loop ([l clauses])
			   (if (null? l)
			       null
			       (syntax-case (car l) ()
				 [(test-expr body ...)
				  (with-syntax ([rest (loop (cdr l))])
				    (let ([test (syntax-e (syntax test-expr))]
					  [body (syntax (body ...))])
				      (cond
				       [(or (string? test) (number? test))
					(with-syntax ([t (make-test test (syntax test-expr))]
						      [body (wrap-state body)])
					  (syntax
					   ((cond [t . body]
						  [else (void)])
					    . rest)))]
				       [(eq? test 'else)
					(with-syntax ([body (wrap-state body)])
					  (syntax
					   ((when else-ready? . body)
					    (set! else-ready? #t)
					    . rest)))]
				       [(eq? test 'range)
					(make-range #f #f body (syntax rest))]
				       [(eq? test ':range)
					(make-range #t #f body (syntax rest))]
				       [(eq? test 'range:)
					(make-range #f #t body (syntax rest))]
				       [(eq? test ':range:)
					(make-range #t #t body (syntax rest))]
				       [(eq? test 'after)
					(syntax rest)]
				       [(eq? test '/)
					(with-syntax ([g (car (generate-temporaries '(1)))])
					  (syntax-case* body (/) (lambda (a b)
								  (eq? (syntax-e a)
								       (syntax-e b)))
					    [(re / (var ...) . body)
					     (and (string? (syntax-e (syntax re)))
						  (andmap (lambda (x) (or (identifier? x)
									  (not (syntax-e x))))
							  (syntax->list (syntax (var ...)))))
					     (with-syntax ([(var ...)
							    (map (lambda (x)
								   (if (identifier? x)
								       x
								       (car (generate-temporaries '(1)))))
								 (syntax->list (syntax (var ...))))]
							   [body (wrap-state (syntax body))])
					       (set! initvars (cons (syntax (g (regexp re))) initvars))
					       (syntax
						((cond
						  [(regexp-match re record)
						   => (lambda (arg)
							(apply
							 (lambda (var ...) . body)
							 arg))]
						  [else (void)])
						 . rest)))]
					    [_else (raise-syntax-error
						    #f
						    "bad / ... / clause"
						    stx
						    (car l))]))]
				       [else
					(with-syntax ([body (wrap-state body)])
					  (syntax
					   ((cond [test-expr . body]
						  [else (void)])
					    . rest)))])))]
				 [_else (raise-syntax-error
					 #f
					 "bad clause"
					 stx
					 (car l))]))))])
	       (with-syntax ([testing-clauses (get-testing-clauses)]
			     [after-clauses (get-after-clauses)]
			     [initvars initvars])
		 (syntax
		  (let ((state-variable init-expr) ...
			. initvars)
		    (let loop ([counter 1]) 
		      (call-with-values (lambda () next-record)
			(lambda (record field ...)
			  (if (eof-object? record)
			      (begin
				. after-clauses)
			      (let ([else-ready? #t])
				(let/ec escape
				  (let ([continue
					 (lambda (local-state ... . extras)
					   (set! state-variable local-state)
					   ...
					   (escape))])
				    . testing-clauses))
				(loop (add1 counter)))))))))))))]
	;; Left out continue...
	[(_ next-record
	    (record field-variable ...)
	    counter-variable
	    ((state-variable init-expr) ...)
	    clause ...)
	 (identifier? (syntax counter-variable))
	 (syntax
	  (awk next-record
	       (record field-variable ...)
	       counter-variable
	       ((state-variable init-expr) ...)
	       continue
	       clause ...))]
	;; Left out counter...
	[(_ next-record
	    (record field-variable ...)
	    ((state-variable init-expr) ...)
	    continue-variable
	    clause ...)
	 (identifier? (syntax continue-variable))
	 (syntax
	  (awk next-record
	       (record field-variable ...)
	       counter
	       ((state-variable init-expr) ...)
	       continue-variable
	       clause ...))]
	;; Left out both...
	[(_ next-record
	    (record field-variable ...)
	    ((state-variable init-expr) ...)
	    clause ...)
	 (syntax
	  (awk next-record
	       (record field-variable ...)
	       counter
	       ((state-variable init-expr) ...)
	       continue
	       clause ...))])))

  (define-struct match (s a))

  (define match:start
    (case-lambda
     [(rec) (match:start rec 0)]
     [(rec which) (car (list-ref (match-a rec) which))]))

  (define match:end
    (case-lambda
     [(rec) (match:end rec 0)]
     [(rec which) (cdr (list-ref (match-a rec) which))]))

  (define match:substring
    (case-lambda
     [(rec) (match:substring rec 0)]
     [(rec which) (let ([p (list-ref (match-a rec) which)])
		    (substring (match-s rec) (car p) (cdr p)))]))

  (define regexp-exec
    (lambda (re s)
      (let ([r (regexp-match-positions re s)])
	(if r
	    (make-match s r)
	    #f)))))
