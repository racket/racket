(module dynamic mzscheme

  (require-for-syntax (lib "stx.ss" "syntax")
		      "private/ops.ss"
		      "private/util.ss"
		      (lib "kerncase.ss" "syntax"))
  
  (begin-for-syntax

   (define kernel-forms (kernel-form-identifier-list #'here))

   (define (top-block-context? ctx) (memq ctx '(top-block)))
   (define (return-block-context? ctx) (memq ctx '(return-block)))
   (define (block-context? ctx) (memq ctx '(top-block block return-block)))
   (define (expression-context? ctx) (memq ctx '(expression)))
   (define (type-context? ctx) (memq ctx '(type)))

   (define block-context 'block)
   (define return-block-context 'return-block)
   (define top-block-context 'top-block)
   (define expression-context 'expression)
   (define type-context 'type)

   ;; --------------------------------------------------------
   ;; Transformer procedure property and basic struct

   (define-values (prop:honu-transformer honu-transformer? honu-transformer-ref)
     (make-struct-type-property 'honu-transformer))


   (define-values (struct:honu-trans make-honu-trans honu-trans? honu-trans-ref honu-trans-set!)
     (make-struct-type 'honu-trans #f 1 0 #f 
		       (list (list prop:honu-transformer #t))
		       (current-inspector) 0))

   (define (make-honu-transformer proc)
     (unless (and (procedure? proc)
		  (procedure-arity-includes? proc 2))
       (raise-type-error
	'define-honu-syntax
	"procedure (arity 2)"
	proc))
     (make-honu-trans proc))

   ;; --------------------------------------------------------
   ;; Type

   (define-values (struct:honu-type make-h-type honu-type? honu-type-ref honu-type-set!)
     (make-struct-type 'honu-type #f 3 0 #f null (current-inspector) 0))

   (define (honu-type-stx v) (honu-type-ref v 0))
   (define (honu-type-pred-def-stx v) (honu-type-ref v 1))
   (define (honu-type-pred-stx v) (honu-type-ref v 2))

   ;; --------------------------------------------------------
   ;; Parsing blocks

   (define operator? 
     (let ([sym-chars (string->list "+-_=?:<>.!%^&*/~|")])
       (lambda (stx)
	 (and (identifier? stx)
	      (let ([str (symbol->string (syntax-e stx))])
		(and (positive? (string-length str))
		     (memq (string-ref str 0) sym-chars)))))))
   
   (define (get-transformer stx)
     (or (and (stx-pair? stx)
	      (identifier? (stx-car stx))
	      (let ([v (syntax-local-value (stx-car stx) (lambda () #f))])
		(and (honu-transformer? v) v)))
	 (and (stx-pair? stx)
	      (let ([first (stx-car stx)])
		(and (stx-pair? first)
		     (identifier? (stx-car first))
		     (module-identifier=? #'#%parens (stx-car first))
		     ;; If the stx-car is a list with just one operator symbol,
		     ;;  try using the operator as a transformer
		     (let ([l (cdr (stx->list first))])
		       (let loop ([l l])
			 (cond
			  [(null? l) #f]
			  [(operator? (car l))
			   (if (ormap operator? (cdr l))
			       #f
			       (let ([v (syntax-local-value (car l) (lambda () #f))])
				 (and (honu-transformer? v)
				      v)))]
			  [else (loop (cdr l))]))))))))
   
   ;; --------------------------------------------------------
   ;; Parsing blocks

   (define parse-an-expr #f)
   (define parse-a-tail-expr #f)

   (define (parse-block-one ctx body k done-k)
     (cond
      [(stx-null? body) (done-k)]
      [(get-transformer body)
       => (lambda (transformer)
	    (let-values ([(code rest) (transformer body ctx)])
	      (k code rest)))]
      [(syntax-case body (#%braces) 
	 [((#%braces . block) . rest) (cons #'block #'rest)]
	 [_else #f])
       => (lambda (b+r)
	    (k #`(honu-unparsed-block #f void-type #f #,(return-block-context? ctx)
				      #,@(car b+r))
	       (cdr b+r)))]
      [else (let-values ([(expr-stxs after-expr) (extract-until body (list #'\;))])
	      (unless expr-stxs
		(raise-syntax-error
		 #f
		 "expected a semicolon to terminate form"
		 (stx-car body)))
	      (let ([code ((if (return-block-context? ctx)
			       parse-a-tail-expr
			       parse-an-expr)
			   expr-stxs)])
		(k ((if (top-block-context? ctx) 
			(lambda (x) `(printf "~s\n" ,x))
			values)
		    code)
		   (stx-cdr after-expr))))]))
   
   (define (parse-block stx ctx)
     (let loop ([stx stx])
       (parse-block-one ctx
			stx 
			(lambda (code rest)
			  (cons code (loop rest)))
			(lambda ()
			  null))))

   ;; --------------------------------------------------------
   ;; Parsing expressions

   (define parse-expr
     (let ()
       (define (parse-expr-seq stx)
	 (define (start-expr stx) 
	   (let ([trans (get-transformer stx)])
	     (if trans
		 (let-values ([(expr rest) (trans stx expression-context)])
		   (if (stx-null? rest)
		       (list expr)
		       (cons expr (start-operator rest))))
		 (syntax-case stx (#%parens #%braces)
		   [(v)
		    (or (number? (syntax-e #'v))
			(identifier? #'v)
			(string? (syntax-e #'v)))
		    (if (operator? #'v)
			(raise-syntax-error
			 #f
			 "operator alone is not an expression"
			 #'v)
			(list #'v))]
		   [((#%parens . pexpr))
		    (if (stx-null? #'pexpr)
			(raise-syntax-error
			 #f
			 "missing expression inside parentheses"
			 (stx-car stx))
			(list (parse-expr #'pexpr)))]
		   [((#%braces . pexpr))
		    (if (stx-null? #'pexpr)
			(raise-syntax-error
			 #f
			 "missing expression inside braces"
			 (stx-car stx))
			(list #'(honu-unparsed-block #f void-type #f #f . pexpr)))]
		   [(op . more)
		    (and (identifier? #'op)
			 (ormap (lambda (uop)
				  (module-identifier=? #'op uop))
				unary-prefix-ops))
		    (cons (make-prefix (stx-car stx)) (start-expr #'more))]
		   [(expr then . more)
		    (append (start-expr (list #'expr))
			    (start-operator #'(then . more)))]
		   [(bad . rest)
		    (raise-syntax-error
		     'expression
		     "unknown expression form"
		     #'bad)]))))
	 (define (start-operator stx)
	   (unless (or (and (stx-pair? (stx-car stx))
			    (or (module-identifier=? #'#%brackets (stx-car (stx-car stx)))
				(module-identifier=? #'#%parens (stx-car (stx-car stx)))))
		       (and (identifier? (stx-car stx))
			    (hash-table-get op-table
					    (syntax-e (stx-car stx))
					    (lambda () #f))))
	     (raise-syntax-error
	      'expression
	      "expected an operator, but found something else"
	      (stx-car stx)))
	   ;; Check for postfix operator, first
	   (cond
	    [(stx-pair? (stx-car stx))
	     ;; Convert vector index or application to a binary operator:
	     (let ([opl (if (module-identifier=? #'#%brackets (stx-car (stx-car stx)))
			    (let ([index-expr (parse-expr (stx-cdr (stx-car stx)))])
			      (list (make-infix (stx-car (stx-car stx)))
				    index-expr))
			    (let ([arg-exprs (parse-arg-list (stx-cdr (stx-car stx)))])
			      (list (make-infix (stx-car (stx-car stx)))
				    arg-exprs)))])
	       (if (stx-null? (stx-cdr stx))
		   opl
		   (append opl (start-operator (stx-cdr stx)))))]
	    [(or (module-identifier=? #'++ (stx-car stx))
		 (module-identifier=? #'-- (stx-car stx)))
	     (if (null? (stx-cdr stx))
		 (list (make-postfix (stx-car stx)))
		 (cons (make-postfix (stx-car stx))
		       (start-operator (stx-cdr stx))))]
	    [else
	     ;; Otherwise, must be infix
	     (cons (make-infix (stx-car stx))
		   (start-expr (stx-cdr stx)))]))
	 (start-expr stx))

       (define (parse-expr stx)
         (let group ([seq (parse-expr-seq stx)])
	   ;; seq is a list that mixes exprs with ops.
	   ;; Find leftmost oper with maximal precedence
	   (if (null? (cdr seq))
	       (car seq)
	       (let loop ([seq seq][before null][op #f][since null])
		 (cond
		  [(null? seq)
		   (cond
		    [(prefix? op)
		     (group (append (reverse (cdr before))
				    (list (quasisyntax/loc (op-id op)
					    (#,(op-id op) #,(car before))))
				    (reverse since)))]
		    [(postfix? op)
		     (let ([after (reverse since)])
		       (group (append (reverse before)
				      (list (quasisyntax/loc (op-id op)
					      (#,(op-id op) #,(car after))))
				      (cdr after))))]
		    [(infix? op)
		     (let ([after (reverse since)])
		       (group (append (reverse (cdr before))
				      (list (quasisyntax/loc (op-id op)
					      (#,(op-id op) #,(car before) #,(car after))))
				      (cdr after))))]
		    [else (error "not an op!: " op)])]
		  [(not (op? (stx-car seq)))
		   (loop (cdr seq) before op (cons (car seq) since))]
		  [(> (hash-table-get precedence-table (prec-key (car seq)) (lambda () 0))
		      (hash-table-get precedence-table (prec-key op) (lambda () 0)))
		   (loop (cdr seq) 
			 (if op
			     (append since (list op) before)
			     since)
			 (car seq) null)]
		  [else
		   (loop (cdr seq) before op (cons (car seq) since))])))))

       (define (parse-arg-list stxs)
	 (if (stx-null? stxs)
	     stxs
	     (let-values ([(val-stxs after-expr) (extract-until stxs (list #'\,))])
	       (when (and val-stxs
			  (stx-null? (stx-cdr after-expr)))
		 (raise-syntax-error
		  'procedure\ call
		  "missing expression after comma"
		  (stx-car after-expr)))
	       (when (null? val-stxs)
		 (raise-syntax-error
		  'procedure\ call
		  "missing expression before token"
		  (stx-car after-expr)))
	       (if val-stxs
		   (cons (parse-expr val-stxs)
			 (parse-arg-list (stx-cdr after-expr)))
		   (list (parse-expr stxs))))))
       
       parse-expr))

   (define (parse-tail-expr expr-stxs)
     (syntax-case expr-stxs (honu-return #%parens)
       [(honu-return expr ...)
	(let ([exprs #'(expr ...)])
	  (when (stx-null? exprs)
	    (raise-syntax-error 
	     #f
	     "missing expression"
	     (stx-car expr-stxs)))
	  (parse-expr exprs))]
       [((#%parens expr0 expr ...))
	(let ([exprs #'(expr0 expr ...)])
	  (parse-tail-expr exprs))]
       [_else
	(parse-expr expr-stxs)]))

   (set! parse-an-expr parse-expr)
   (set! parse-a-tail-expr parse-tail-expr)

   ;; --------------------------------------------------------
   ;; Parsing declarations (which always start with a type)

   (define (parse-one-argument proc-id type id k)
     (cons (list id 
		 (honu-type-stx type)
		 (honu-type-pred-def-stx type)
		 (honu-type-pred-stx type))
	   (k)))

   (define (parse-arguments orig-args-stx proc-id)
     (if (stx-null? orig-args-stx)
	 null
	 (let loop ([args-stx orig-args-stx]
		    [where "at start of argument sequence"]
		    [where-stx orig-args-stx])
	   (let-values ([(type rest-stx) (if (syntax-case args-stx (\,)
					       [(id \, . rest)
						(identifier? #'id)
						#t]
					       [(id)
						(identifier? #'id)
						#t]
					       [_else #f])
					     (values (make-h-type #'val #'(begin) #'(lambda (x) (values #t x)))
						     args-stx)
					     (let ([trans (get-transformer args-stx)])
					       (if trans
						   (trans args-stx type-context)
						   (values #f #f))))])
	     (unless (honu-type? type)
	       (raise-syntax-error
		'|procedure declaration|
		(format "expected a type ~a" where)
		where-stx))
	     (syntax-case rest-stx ()
	       [(id)
		(identifier? #'id)
		(parse-one-argument proc-id type #'id
				    (lambda () null))]
	       [(id comma . rest)
		(and (identifier? #'id)
		     (identifier? #'comma)
		     (module-identifier=? #'comma #'\,))
		(parse-one-argument proc-id type #'id
				    (lambda ()
				      (loop #'rest
					    "after comma"
					    #'comma)))]
	       [(id something . rest)
		(identifier? #'id)
		(raise-syntax-error
		 'procedure\ declaration
		 "expected a comma after identifier name"
		 #'something)]
	       [_else
		(raise-syntax-error
		 'procedure\ declaration
		 "expected an argument identifier"
		 (car rest-stx))])))))

   (define (make-honu-type pred-id mk-pred-def only-mode)
     (make-honu-trans
      (lambda (orig-stx ctx)
	(let* ([pred-id (or pred-id
			    (car (generate-temporaries '(type-pred))))]
	       [pred-def (if mk-pred-def
			     (mk-pred-def pred-id orig-stx)
			     #'(begin))])
	  (cond
	   [(block-context? ctx)
	    (with-syntax ([pred-id pred-id]
			  [type-name (stx-car orig-stx)])
	      (let loop ([stx (stx-cdr orig-stx)]
			 [after (stx-car orig-stx)]
			 [after-what "type name"]
			 [parens-ok? #t])
		(syntax-case stx ()
		  [(id . rest)
		   (begin
		     (unless (identifier? #'id)
		       (raise-syntax-error 'declaration
					   (format "expected a identifier after ~a" after-what)
					   (stx-car orig-stx)
					   #'id))
		     (if (and (identifier? (stx-car #'rest))
			      (module-identifier=? #'set! (stx-car #'rest)))
			 ;; -- Non-procedure declaration
			 (if (eq? 'function only-mode)
			     (raise-syntax-error 
			      'declaration
			      "expected parentheses after name for function definition"
			      (stx-car #'rest))
			     (let-values ([(val-stxs after-expr) (extract-until (stx-cdr #'rest)
										(list #'\; #'\,))])
			       (unless val-stxs
				 (raise-syntax-error 
				  'declaration
				  "missing semicolon or comma after initializing assignment"
				  (stx-car #'rest)))
			       (when (null? val-stxs)
				 (raise-syntax-error 
				  'declaration
				  "missing expression initializing assignment"
				  (stx-car #'rest)))
			       (let ([def #`(define-typed id #f type-name pred-id 
					      (check-expr #f 'id type-name pred-id 
							  (honu-unparsed-expr #,@val-stxs)))])
				 (if (module-identifier=? #'\; (stx-car after-expr))
				     (values #`(begin #,pred-def #,def) (stx-cdr after-expr))
				     (let-values ([(defs remainder kind) (loop (stx-cdr after-expr) (stx-car after-expr) "comma" #f)])
				       (values #`(begin #,pred-def #,def #,defs) remainder))))))
			 ;; -- Procedure declaration
			 (if (eq? 'var only-mode)
			     (raise-syntax-error 
			      'declaration
			      "expected = after name for variable"
			      (stx-car #'rest))
			     (syntax-case #'rest (#%parens \;)
			       [((#%parens . prest) (#%braces . body) . rest)
				parens-ok?
				(let ([args (parse-arguments #'prest #'id)])
				  (with-syntax ([((arg arg-type arg-pred-def arg-pred-id) ...) args]
						[(temp-id ...) (generate-temporaries (map car args))])
				    (values #`(begin
						#,pred-def
						arg-pred-def ...
						(define-typed-procedure id 
						  ((arg arg-type arg-pred-id) ...)
						  (lambda (temp-id ...)
						    (define-typed arg id arg-type arg-pred-id temp-id) ...
						    (honu-unparsed-block id type-name pred-id #t . body))))
					    #'rest)))]
			       ;; --- Error handling ---
			       [((#%parens . prest) . bad-rest)
				parens-ok?
				(begin
				  (parse-arguments #'prest #'id)
				  (raise-syntax-error 
				   '|procedure declaration|
				   "braces for function body after parenthesized arguments"
				   (stx-car #'rest)
				   #'id))]
			       [_else
				(raise-syntax-error 
				 '|declaration|
				 (if parens-ok?
				     "expected either = (for variable intialization) or parens (for function arguments)"
				     "expected = (for variable initialization)")
				 #'id)]))))]
		  [_else
		   (raise-syntax-error #f 
				       (format "expected a identifier after ~a" after-what)
				       after
				       #'id)])))]
	   [only-mode
	    (raise-syntax-error #f 
				(format "illegal in an ~a context"
					(if (type-context? ctx) 
					    "type"
					    "expression"))
				(stx-car orig-stx))]
	   [(type-context? ctx) 
	    (values (make-h-type (stx-car orig-stx) pred-def pred-id) (stx-cdr orig-stx))]
	   [(expression-context? ctx)
	    (raise-syntax-error #f 
				"illegal in an expression context"
				(stx-car orig-stx))])))))

   (define (make-proc-predicate name form)
     ;; Form start with a operator-transformer sequence
     (let-values ([(args-stx -> result-stx) 
		   (let loop ([stx (stx-cdr (stx-car form))][args null])
		     (if (and (identifier? (stx-car stx))
			      (module-identifier=? #'-> (stx-car stx)))
			 (values (reverse args) (stx-car stx) (stx-cdr stx))
			 (loop (stx-cdr stx) (cons (stx-car stx) args))))])
       (when (stx-null? result-stx)
	 (raise-type-error
	  #f
	  "missing type for result"
	  ->))
       (let ([arg-types
	      (let loop ([args-stx args-stx])
		(if (stx-null? args-stx)
		    null
		    (let ([trans (get-transformer args-stx)])
		      (unless trans
			(raise-type-error '->
					  "non-type within a procedure-type construction"
					  (stx-car args-stx)))
		      (let-values ([(type rest-stx) (trans args-stx type-context)])
			(cons type (loop rest-stx))))))]
	     [result-type 
	      (let ([trans (get-transformer result-stx)])
		(unless trans
		  (raise-type-error '->
				    "non-type in result position for procedure-type construction"
				    (stx-car result-stx)))
		(let-values ([(type rest-stx) (trans result-stx type-context)])
		  (unless (stx-null? rest-stx)
		    (raise-type-error '->
				      "extra tokens following result for procedure-type construction"
				      (stx-car rest-stx)))
		  type))])
	 (with-syntax ([(arg ...) (generate-temporaries arg-types)]
		       [(arg-type ...) (map honu-type-stx arg-types)]
		       [(arg-pred-def ...) (map honu-type-pred-def-stx arg-types)]
		       [(arg-pred-id ...) (map honu-type-pred-stx arg-types)]
		       [result-type (honu-type-stx result-type)]
		       [result-pred-def (honu-type-pred-def-stx result-type)]
		       [result-pred-id (honu-type-pred-stx result-type)]
		       [n (length arg-types)])
	   #`(begin
	       arg-pred-def ...
	       result-pred-def
	       (define (#,name v)
		 (if (and (procedure? v)
			  (procedure-arity-includes? v n))
		     (values #t (lambda (arg ...)
				  (check-expr
				   #f #t result-type result-pred-id
				   (v (check-expr #f #f arg-type arg-pred-id arg) ...))))
		     (values #f #f))))))))
       
   (define (compatible-type? val-expr val-type target-type)
     (and (identifier? target-type)
	  (identifier? val-type)
	  (or (module-identifier=? val-type target-type)
	      (module-identifier=? #'val target-type)
	      (and (number? (syntax-e val-expr))
		   (module-identifier=? #'num target-type))
	      (and (integer? (syntax-e val-expr))
		   (exact? (syntax-e val-expr))
		   (module-identifier=? #'int target-type))
	      (and (real? (syntax-e val-expr))
		   (module-identifier=? #'real target-type))
	      (and (string? (syntax-e val-expr))
		   (module-identifier=? #'string-type target-type))))))
      
   (define (check proc who type-name pred val)
     (let-values ([(tst new-val) (pred val)])
       (unless tst
	 (raise
	  (make-exn:fail:contract
	   (string->immutable-string
	    (format "~a: expected ~a value for ~a, got something else: ~e"
		    (or proc (if (eq? who #t) #f who) "procedure")
		    type-name
		    (cond
		     [(eq? who #t) "result"]
		     [else (if proc 
			       (format "~a argument" who)
			       (if who
				   "initialization"
				   "argument"))])
		    val))
	   (current-continuation-marks))))
       new-val))

  (define-syntax (check-expr stx)
    (syntax-case stx ()
      [(_ proc who type-name pred val)
       ;; Avoid the check if the static types are consistent
       (let ([v (local-expand
		 #'val
		 'expression
		 (cons #'honu-typed
		       kernel-forms))])
	 (syntax-case v (honu-typed)
	   [(honu-typed val val-type)
	    (compatible-type? #'val #'val-type #'type-name)
	    ;; No run-time check:
	    #'val]
	   [_else
	    ;; Even without a type for v, we might see a literal,
	    ;;  or maybe the declaration is simply val
	    (if (compatible-type? v #'val #'type-name)
		;; No run-time check:
		#'val
		;; Run-time check:
		#'(check proc who 'type-name pred val))]))]))

  (define-syntax (define-typed stx)
    (syntax-case stx ()
      [(_ id proc-name type-name pred-id val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define gen-id val)
	     (define-syntax id
	       (make-set!-transformer
		(lambda (stx)
		  (syntax-case stx (set!)
		    [(set! id rhs)
		     #'(set! gen-id (check-expr set! id type-name pred-id rhs))]
		    [(id arg (... ...))
		     #'(#%app (honu-typed gen-id type-name) arg (... ...))]
		    [id
		     #'(honu-typed gen-id type-name)]))))))]))

  (define-syntax (define-typed-procedure stx)
    (syntax-case stx ()
      [(_ id arg-spec val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define gen-id val)
	     (define-syntax id
	       (with-syntax ([((arg arg-type pred-id) (... ...)) (quote-syntax arg-spec)])
		 (make-set!-transformer
		  (lambda (stx)
		    (syntax-case stx (set!)
		      [(set! id rhs)
		       (raise-syntax-error #f
					   "cannot assign to procedure name"
					   stx
					   #'id)]
		      [(id actual-arg (... ...))
		       (let ([actual-args (syntax->list #'(actual-arg (... ...)))]
			     [formal-args (syntax->list #'(arg (... ...)))])
			 (unless (= (length actual-args)
				    (length formal-args))
			   (raise-syntax-error
			    'id
			    (format "expects ~a arguments, provided ~a"
				    (length formal-args)
				    (length actual-args))
			    stx))
			 #'(#%app (honu-typed gen-id type-name) 
				  (check-expr 'id 'arg arg-type pred-id actual-arg) 
				  (... ...)))]
		      [id
		       #'(honu-typed (let ([id (lambda (arg (... ...))
						 (id arg (... ...)))])
				       id)
				     type-name)])))))))]))

  (define-syntax honu-typed
    (syntax-rules ()
      [(_ expr type) expr]))

  (require-for-syntax (lib "context.ss" "syntax"))
  (define-syntax (honu-block stx)
    ;; A block can have mixed exprs and defns. Wrap expressions with
    ;; `(define-values () ... (values))' as needed, and add a (void)
    ;; at the end if needed. Also, wrap the final expression with
    ;; a type check as needed.
    (let ([proc-id (stx-car (stx-cdr stx))]
	  [result-type-name (stx-car (stx-cdr (stx-cdr stx)))]
	  [result-pred-id (stx-car (stx-cdr (stx-cdr (stx-cdr stx))))]
	  [exprs (let loop ([exprs (cddddr (syntax->list stx))])
		   (apply 
		    append
		    (map (lambda (expr)
			   (let ([expr (local-expand
					expr
					(generate-expand-context)
					kernel-forms)])
			     (syntax-case expr (begin)
			       [(begin . rest)
				(loop (syntax->list #'rest))]
			       [else
				(list expr)])))
			 exprs)))])
      #`(let ()
	  #,@(let loop ([exprs exprs][prev-defns null][prev-exprs null])
	       (cond
		[(null? exprs) (append 
				(reverse prev-defns)
				(if (and (pair? prev-exprs) 
					 proc-id 
					 (syntax-e proc-id))
				    (reverse (cons
					      #`(check-expr '#,proc-id #t
							    #,result-type-name 
							    #,result-pred-id 
							    #,(car prev-exprs))
					      (cdr prev-exprs)))
				    (begin
				      (unless (or (not proc-id)
						  (not (syntax-e proc-id))
						  (module-identifier=? #'type-name #'void-type))
					(error "no expression for type check; should have been "
					       "caught earlier"))
				      (reverse prev-exprs)))
				(if (null? prev-exprs)
				    (list #'(void))
				    null))]
		[(and (stx-pair? (car exprs))
		      (or (module-identifier=? #'define-values (stx-car (car exprs)))
			  (module-identifier=? #'define-syntaxes (stx-car (car exprs)))))
		 (loop (cdr exprs)
		       (cons (car exprs)
			     (append
			      (map (lambda (expr)
				     #`(define-values () (begin #,expr (values))))
				   prev-exprs)
			      prev-defns))
		       null)]
		[else
		 (loop (cdr exprs) prev-defns (cons (car exprs) prev-exprs))])))))

  (define-syntax (honu-unparsed-block stx)
    (syntax-case stx (void)
      [(_ proc-id result-type-name result-pred-id return-context? . body) 
       #`(honu-block proc-id result-type-name result-pred-id #,@(parse-block 
								 #'body
								 (if (syntax-e #'return-context?)
								     return-block-context
								     block-context)))]))

  (define-syntax (honu-unparsed-expr stx)
    (syntax-case stx ()
      [(_ v ...) (parse-expr (syntax->list #'(v ...)))]))

  (define-syntax (h-return stx)
    (syntax-case stx ()
      [(_ expr) #'expr]))

  (define-syntax (#%parens stx)
    (syntax-case stx ()
      [(_ rator (rand ...)) (syntax/loc #'rator (rator rand ...))]))

  ;; --------------------------------------------------------
  ;; Defining a new transformer or new type

  (require-for-syntax (lib "define.ss" "syntax"))
  (define-syntax (define-honu-syntax stx)
    (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
      (with-syntax ([id id]
		    [rhs rhs])
	#'(define-syntax id (make-honu-transformer rhs)))))

  (define-syntax (define-type stx)
    (syntax-case stx ()
      [(_ id pred-expr)
       (identifier? #'id)
       (with-syntax ([pred-id (car (generate-temporaries '(pred)))])
	 #'(begin
	     (define pred-id (let ([pred pred-expr])
				(lambda (v)
				  (values (pred v) v))))
	     (define-syntax id (make-honu-type #'pred-id #f #f))))]))

  (define-syntax (define-type-constructor stx)
    (syntax-case stx ()
      [(_ id generator-expr)
       (identifier? #'id)
       #'(define-syntax id (make-honu-type #f generator-expr #f))]))

  ;; ----------------------------------------
  ;;  Pre-defined types and forms

  (define (exact-integer? v)
    (and (integer? v) (exact? v)))

  (define-type int exact-integer?)
  (define-type real real?)
  (define-type num number?)
  (define-type obj (lambda (x) #t))
  (define-type string-type string?)

  (define-syntax function (make-honu-type #'(lambda (x) (values #t x)) #f 'function))
  (define-syntax var (make-honu-type #'(lambda (x) (values #t x)) #f 'var))

  (define-type-constructor -> make-proc-predicate)

  (define-for-syntax parse-comma-separated
    (lambda (body empty-case parse-one combine)
      (syntax-case body (\;)
	[(\;) (empty-case)]
	[_else
	 (let loop ([body body][accum null][prev-comma #f])
	   (syntax-case body (\, \;)
	     [(\, . rest)
	      (let-values ([(one) (parse-one (reverse accum) prev-comma (stx-car body))]
			   [(other rest) (loop #'rest null (stx-car body))])
		(values (combine one other) rest))]
	     [(\; . rest)
	      (identifier? #'id)
	      (values (parse-one (reverse accum) prev-comma (stx-car body)) #'rest)]
	     [(x . rest)
	      (loop #'rest (cons #'x accum) #f)]))])))

  (define-honu-syntax honu-provide
    (lambda (body ctx)
      (unless (top-block-context? ctx)
	(raise-syntax-error #f "not allowed outside the top level" (stx-car body)))
      (parse-comma-separated
       (stx-cdr body)
       (lambda () #'(begin))
       (lambda (stxes prev-comma-stx term-stx)
	 (syntax-case stxes ()
	   [(id)
	    (identifier? #'id)
	    #`(provide id)]
	   [else
	    (raise-syntax-error
	     #f
	     "unknown provide form"
	     (stx-car body)
	     (car stxes))]))
       (lambda (p decls)
	 #`(begin #,p #,decls)))))
  
  (define-honu-syntax honu-require
    (lambda (body ctx)
      (define (check-empty rest after-what)
	(unless (stx-null? rest)
	  (raise-syntax-error
	   #f
	   (format "expect a comma or semicolon after ~a" after-what)
	   (stx-car body)
	   (stx-car rest))))
      (unless (top-block-context? ctx)
	(raise-syntax-error #f "not allowed outside the top level" (stx-car body)))
      (parse-comma-separated
       (stx-cdr body)
       (lambda () #'(begin))
       (lambda (stxes prev-comma-stx term-stx)
	 #`(require 
	    #,(let ()
		(define (parse-module-name stxes)
		  (syntax-case stxes (lib file #%parens)
		    [(fn . rest)
		     (string? (syntax-e #'fn))
		     (begin
		       (check-empty #'rest "path string")
		       #'fn)]
		    [(lib (#%parens names ...) . rest)
		     (let ([names (let loop ([names #'(names ...)])
				    (syntax-case names (\,)
				      [() null]
				      [(name . rest)
				       (begin
					 (unless (string? (syntax-e #'name))
					   (raise-syntax-error
					    #f
					    "expected a string for a library path"
					    (car stxes)
					    #'name))
					 (syntax-case #'rest (\,)
					   [() (list #'name)]
					   [(\, . rest)
					    (cons #'name (loop #'rest))]
					   [else
					    (raise-syntax-error
					     #f
					     "expected a comma"
					     (stx-car stxes)
					     (stx-car #'rest))]))]
				      [(\,)
				       (raise-syntax-error
					#f
					"expected a string before comma"
					(car stxes)
					(stx-car names))]
				      [_else
				       (raise-syntax-error
					#f
					"expected a string for a library path"
					(car stxes)
					(stx-car names))]))])
		       (when (null? names)
			 (raise-syntax-error
			  #f
			  "expected at least one string for the library path"
			  (cadr stxes)))
		       (check-empty #'rest "library path")
		       (syntax-local-introduce #`(lib #,@names)))]
		    [(lib . rest)
		     (raise-syntax-error 
		      #f
		      "expected a parenthesized sequence of strings after `lib' keyword"
		      (car stxes)
		      (stx-car body))]
		    [(file (#%parens name) . rest)
		     (string? (syntax-e #'name))
		     (begin
		       (check-empty #'rest "file name")
		       (syntax-local-introduce #`(file name)))]
		    [(file . rest)
		     (raise-syntax-error 
		      #f
		      "expected a parenthesized string after `file' keyword"
		      (car stxes)
		      (stx-car body))]
		    [(fn)
		     (identifier? #'fn)
		     #'fn]
		    [else
		     (raise-syntax-error
		      #f
		      "unknown require form"
		      (stx-car body)
		      (car stxes))]))
		(define (parse-module-spec stxes)
		  (syntax-case stxes (rename #%parens \,)
		    [(rename (#%parens spec0 spec ... \, local-id \, remote-id) . rest)
		     (begin
		       (unless (identifier? #'local-id)
			 (raise-syntax-error
			  #f
			  "expected an identifier"
			  (stx-car stxes)
			  #'local-id))
		       (unless (identifier? #'remote-id)
			 (raise-syntax-error
			  #f
			  "expected an identifier"
			  (stx-car stxes)
			  #'remote-id))
		       (begin0
			#`(rename #,(parse-module-name 
				     (syntax->list #'(spec0 spec ...))) 
				  local-id 
				  remote-id)
			(check-empty #'rest "rename")))]
		    [(rename . rest)
		     (raise-syntax-error 
		      #f
		      "expected a parenthesized id, id, and require spec `rename' keyword"
		      (car stxes)
		      (stx-car body))]
		    [_else (parse-module-name stxes)]))
		(parse-module-spec stxes))))
       (lambda (p decls)
	 #`(begin #,p #,decls)))))

  (define-honu-syntax honu-return
    (lambda (stx ctx)
      (unless (return-block-context? ctx)
	(raise-syntax-error #f "allowed only in a tail position" (stx-car stx)))
      (let-values ([(val-stxs after-expr) (extract-until (stx-cdr stx)
							 (list #'\;))])
	(unless val-stxs
	  (raise-syntax-error 
	   #f
	   "missing semicolon"
	   (stx-car stx)))
	(when (null? val-stxs)
	  (raise-syntax-error 
	   #f
	   "missing expression"
	   (stx-car stx)))
	(with-syntax ([expr (parse-expr val-stxs)])
	  (unless (stx-null? (stx-cdr after-expr))
	    (raise-syntax-error 
	     #f
	     "not at a block end"
	     (stx-car stx)))
	  (values
	   (syntax/loc (stx-car stx)
	     (h-return expr))
	   null)))))

  (define-honu-syntax honu-if
    (lambda (stx ctx)
      (define (get-block-or-statement kw rest)
	(syntax-case rest (#%braces)
	  [((#%braces then ...) . rest)
	   (values #`(honu-unparsed-block #f void-type #f #,(return-block-context? ctx) then ...)
		   #'rest)]
	  [else
	   (let-values ([(val-stxs rest) (extract-until rest
							(list #'\;))])
	     (unless val-stxs
	       (raise-syntax-error
		#f
		"expected a braced block or a terminating semicolon"
		kw))
	     (when (null? val-stxs)
	       (raise-syntax-error
		#f
		"expected an expression before semicolon"
		kw
		(stx-car rest)))
	     (if (return-block-context? ctx)
		 (values (parse-tail-expr val-stxs) (stx-cdr rest))
		 (values (parse-expr val-stxs) (stx-cdr rest))))]))

      (syntax-case stx (#%parens)
	[(_ (#%parens test ...) . rest)
	 (let ([test-expr (parse-expr (syntax->list #'(test ...)))])
	   (let-values ([(then-expr rest) (get-block-or-statement (stx-car stx) #'rest)])
	     (syntax-case rest (else)
	       [(else . rest2)
		(let-values ([(else-expr rest) (get-block-or-statement (stx-car rest) #'rest2)])
		  (values #`(if #,test-expr #,then-expr #,else-expr)
			  rest))]
	       [_else
		(values #`(if #,test-expr #,then-expr) rest)])))]
	[_else
	 (raise-syntax-error
	  #f
	  "expected a parenthesized test after `if' keyword"
	  (stx-car stx))])))

  ;; ----------------------------------------
  ;; Main compiler loop

  (define-syntax (honu-unparsed-begin stx)
    (syntax-case stx ()
      [(_) #'(begin)]
      [(_ . body) (let-values ([(code rest) (parse-block-one top-block-context
							     #'body 
							     values
							     (lambda ()
							       (values #'(void) null)))])
		    #`(begin
			#,code
			(honu-unparsed-begin #,@rest)))]))

  (define-syntax (#%dynamic-honu-module-begin stx)
    #`(#%plain-module-begin
	(honu-unparsed-begin #,@(stx-cdr stx))))
  
  (define-syntax (\; stx) (raise-syntax-error '\; "out of context" stx))
  
  (define true #t)
  (define false #f)

  (provide int real obj 
	   function var
	   (rename string-type string) ->
	   \;
           (rename set! =)
	   (rename honu-return return)
	   (rename honu-if if)
	   + - * / (rename modulo %)
	   (rename string->number stringToNumber)
	   (rename number->string numberToString)
	   cons list
	   (rename car first) 
	   (rename cdr rest)
	   (rename null empty)
	   (rename null? isEmpty)
	   (rename pair? isCons)
	   true false
	   display write newline
	   #%datum
	   #%top
	   #%parens
	   (rename #%dynamic-honu-module-begin #%module-begin)
	   define-honu-syntax
           (rename honu-provide provide)
           (rename honu-require require)))
