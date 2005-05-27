
;; Used by ../shared.ss, and also collects/lang/private/teach.ss
;; Besides the usual things, this code expects `undefined' and
;; `the-cons' to be bound, and it expects `struct-declaration-info?'
;; from the "struct.ss" library of the "syntax" collection.

(syntax-case stx ()
  [(_ ([name expr] ...) body1 body ...)
   (let ([names (syntax->list (syntax (name ...)))]
	 [exprs (syntax->list (syntax (expr ...)))])
     (for-each (lambda (name)
		 (unless (identifier? name)
		   (raise-syntax-error
		    'shared
		    "not an identifier"
		    stx
		    name)))
	       names)
     (let ([dup (check-duplicate-identifier names)])
       (when dup
	 (raise-syntax-error
	  'shared
	  "duplicate identifier"
	  stx
	  dup)))
     (let ([exprs (map (lambda (expr)
			 (let ([e (local-expand
				   expr
				   'expression
				   (append
				    (kernel-form-identifier-list (quote-syntax here))
				    names))])
			   ;; Remove #%app if present...
			   (syntax-case e (#%app)
			     [(#%app a ...)
			      (syntax/loc e (a ...))]
			     [_else e])))
		       exprs)]
	   [struct-decl-for (lambda (id)
			      (and (identifier? id)
				   (let* ([s (symbol->string (syntax-e id))]
					  [m (regexp-match-positions "make-" s)])
				     (and m
					  (let ([name (datum->syntax-object
						       id
						       (string->symbol (string-append (substring s 0 (caar m))
										      (substring s (cdar m) (string-length s))))
						       id)])
					    (let ([v (syntax-local-value name (lambda () #f))])
					      (and v
						   (struct-declaration-info? v)
						   v)))))))]
	   [same-special-id? (lambda (a b)
			       ;; Almost module-or-top-identifier=?,
			       ;; but handle the-cons specially
			       (or (module-identifier=? a b)
				   (module-identifier=? 
				    a 
				    (datum->syntax-object
				     #f
				     (if (eq? 'the-cons (syntax-e b))
					 'cons
					 (syntax-e b))))))])
       (with-syntax ([(init-expr ...)
		      (map (lambda (expr)
			     (define (bad n)
			       (raise-syntax-error
				'shared
				(format "illegal use of ~a" n)
				stx
				expr))
			     (syntax-case* expr (the-cons list box vector) same-special-id?
			       [(the-cons a d)
				(syntax (cons undefined undefined))]
			       [(the-cons . _)
				(bad "cons")]
			       [(list e ...)
				(with-syntax ([(e ...)
					       (map (lambda (x) (syntax undefined))
						    (syntax->list (syntax (e ...))))])
				  (syntax (list e ...)))]
			       [(list . _)
				(bad "list")]
			       [(box v)
				(syntax (box undefined))]
			       [(box . _)
				(bad "box")]
			       [(vector e ...)
				(with-syntax ([(e ...)
					       (map (lambda (x) (syntax undefined))
						    (syntax->list (syntax (e ...))))])
				  (syntax (vector e ...)))]
			       [(vector . _)
				(bad "vector")]
			       [(make-x . _)
				(struct-decl-for (syntax make-x))
				(let ([decl (struct-decl-for (syntax make-x))]
				      [args (syntax->list (syntax _))])
				  (unless args
				    (bad "structure constructor"))
				  (when (or (not (cadr decl))
					    (ormap not (list-ref decl 4)))
				    (raise-syntax-error
				     'shared
				     "not enough information about the structure type in this context"
				     stx
				     expr))
				  (unless (= (length (list-ref decl 4)) (length args))
				    (raise-syntax-error
				     'shared
				     (format "wrong argument count for structure constructor; expected ~a, found ~a"
					     (length (list-ref decl 4)) (length args))
				     stx
				     expr))
				  (with-syntax ([undefineds (map (lambda (x) (syntax undefined)) args)])
				    (syntax (make-x . undefineds))))]
			       [_else
				expr]))
			   exprs)]
		     [(finish-expr ...)
		      (let ([gen-n (lambda (l)
				     (let loop ([l l][n 0])
				       (if (null? l)
					   null
					   (cons (datum->syntax-object (quote-syntax here) n #f)
						 (loop (cdr l) (add1 n))))))])
			(map (lambda (name expr)
			       (with-syntax ([name name])
				 (syntax-case* expr (the-cons list box vector) same-special-id?
				   [(the-cons a d)
				    (syntax (begin 
					      (set-car! name a)
					      (set-cdr! name d)))]
				   [(list e ...)
				    (with-syntax ([(n ...) (gen-n (syntax->list (syntax (e ...))))])
				      (syntax (let ([lst name])
						(set-car! (list-tail lst n) e)
						...)))]
				   [(box v)
				    (syntax (set-box! name v))]
				   [(vector e ...)
				    (with-syntax ([(n ...) (gen-n (syntax->list (syntax (e ...))))])
				      (syntax (let ([vec name])
						(vector-set! vec n e)
						...)))]
				   [(make-x e ...)
				    (struct-decl-for (syntax make-x))
				    (let ([decl (struct-decl-for (syntax make-x))])
				      (with-syntax ([(setter ...) (reverse (list-ref decl 4))])
					(syntax
					 (begin
					   (setter name e) ...))))]
				   [_else (syntax (void))])))
			     names exprs))]
		     [(check-expr ...)
		      (if make-check-cdr
			  (map (lambda (name expr)
				 (syntax-case* expr (the-cons list box vector) same-special-id?
				   [(the-cons a d)
				    (make-check-cdr name)]
				   [_else (syntax #t)]))
			       names exprs)
			  null)])
	 (syntax
	  (letrec ([name init-expr] ...)
	    finish-expr
	    ...
	    check-expr
	    ...
	    body1
	    body
	    ...)))))])
