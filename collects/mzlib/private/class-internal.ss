
(module class-internal mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
	   (lib "stxparam.ss")
           "class-events.ss"
	   "serialize-structs.ss")
  (require-for-syntax (lib "kerncase.ss" "syntax")
		      (lib "stx.ss" "syntax")
		      (lib "name.ss" "syntax")
		      (lib "context.ss" "syntax")
		      (lib "define.ss" "syntax")
		      (lib "boundmap.ss" "syntax" "private")
		      (lib "stxparam.ss")
		      "classidmap.ss")

  (define insp (current-inspector)) ; for all opaque structures

  ;;--------------------------------------------------------------------
  ;;  keyword setup
  ;;--------------------------------------------------------------------
  
  (define-syntax provide-class-keyword
    (syntax-rules ()
      [(_ id ...)
       (begin
	 (define-syntax (id stx)
	   (if (identifier? stx)
	       (raise-syntax-error
		#f
		"illegal (unparenthesized) use of a class keyword"
		stx)
	       (raise-syntax-error
		#f
		"use of a class keyword is not in a class top-level"
		stx)))
	 ...
	 (provide id ...))]))
  
  (provide-class-keyword private public override augride
			 pubment overment augment
			 public-final override-final augment-final
			 rename-super rename-inner inherit inherit-field
                         inherit/super inherit/inner
			 inspect
                         init-rest)

  (define-syntax provide-class-define-like-keyword
    (syntax-rules ()
      [(_ [internal-id id] ...)
       (begin
	 (define-syntax (internal-id stx)
	   (syntax-case stx ()
	     [(_ orig . __)
	      (raise-syntax-error
	       #f
	       "use of a class keyword is not in a class top-level"
	       #'orig)]))
	 ...
	 (define-syntax (id stx)
	   (syntax-case stx ()
	     [(_ elem (... ...))
	      (syntax-property
	       #`(internal-id #,stx
			      #,@(map (lambda (e)
					(if (identifier? e)
					    e
					    (syntax-property
					     (syntax-case e ()
					       [((n1 n2) . expr)
						(quasisyntax/loc e
						  (#,(syntax-property
						      #'(n1 n2)
						      'certify-mode 'transparent)
						   . expr))]
					       [_else e])
					     'certify-mode 'transparent)))
				      (syntax-e #'(elem (... ...)))))
	       'certify-mode
	       'transparent)]
	     [(_ . elems)
	      #`(internal-id #,stx . elems)]
	     [_else 
	      (raise-syntax-error #f "illegal (unparenthesized) use of class keyword" stx)]))
	 ...
	 (provide id ...))]))
  
  (provide-class-define-like-keyword 
   [-field field]
   [-init init]
   [-init-field init-field])

  (define-syntax define/provide-context-keyword
    (syntax-rules ()
      [(_ (id param-id) ...)
       (begin
	 (begin
	   (provide id)
	   (define-syntax-parameter param-id 
	     (make-set!-transformer
	      (lambda (stx)
		(raise-syntax-error
		 #f
		 "use of a class keyword is not in a class"
		 stx))))
	   (define-syntax id
	     (make-parameter-rename-transformer #'param-id)))
	 ...)]))

  (define/provide-context-keyword
    [this this-param]
    [super super-param]
    [inner inner-param]
    [super-make-object super-make-object-param]
    [super-instantiate super-instantiate-param]
    [super-new super-new-param])

  ;;--------------------------------------------------------------------
  ;;  class macros
  ;;--------------------------------------------------------------------

  (define-syntaxes (class* _class class/derived
                     class*-traced class-traced class/derived-traced)
    (let ()
      ;; Start with Helper functions

      (define (expand-all-forms stx defn-and-exprs def-ctx bind-local-id)
	(let* ([stop-forms
		(append
		 (kernel-form-identifier-list (quote-syntax here))
		 (list 
		  (quote-syntax -init)
		  (quote-syntax init-rest)
		  (quote-syntax -field)
		  (quote-syntax -init-field)
		  (quote-syntax inherit-field)
		  (quote-syntax private)
		  (quote-syntax public)
		  (quote-syntax override)
		  (quote-syntax augride)
		  (quote-syntax public-final)
		  (quote-syntax override-final)
		  (quote-syntax augment-final)
		  (quote-syntax pubment)
		  (quote-syntax overment)
		  (quote-syntax augment)
		  (quote-syntax rename-super)
		  (quote-syntax inherit)
		  (quote-syntax inherit/super)
		  (quote-syntax inherit/inner)
		  (quote-syntax rename-inner)
		  (quote-syntax super)
		  (quote-syntax inner)
		  (quote-syntax this)
		  (quote-syntax super-instantiate)
		  (quote-syntax super-make-object)
		  (quote-syntax super-new)
		  (quote-syntax inspect)))]
	       [expand-context (generate-class-expand-context)]
	       [expand
		(lambda (defn-or-expr)
		  (local-expand
		   defn-or-expr
		   expand-context
		   stop-forms
		   def-ctx))])
	  (let loop ([l defn-and-exprs])
	    (if (null? l)
		null
		(let ([e (expand (car l))])
		  (syntax-case e (begin define-syntaxes define-values)
		    [(begin expr ...)
		     (loop (append
			    (syntax->list (syntax (expr ...)))
			    (cdr l)))]
		    [(define-syntaxes (id ...) rhs)
		     (andmap identifier? (syntax->list #'(id ...)))
		     (begin
		       (with-syntax ([rhs (local-transformer-expand
					   #'rhs
					   'expression
					   null)])
			 (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs def-ctx)
			 (cons #'(define-syntaxes (id ...) rhs) (loop (cdr l)))))]
		    [(define-values (id ...) rhs)
		     (andmap identifier? (syntax->list #'(id ...)))
		     (begin
		       (map bind-local-id (syntax->list #'(id ...)))
		       (cons e (loop (cdr l))))]
		    [(begin . _)
		     (raise-syntax-error 
		      #f
		      "ill-formed begin expression"
		      e)]
		    [_else (cons e (loop (cdr l)))]))))))

      ;; returns two lists: expressions that start with an identifier in
      ;; `kws', and expressions that don't
      (define (extract kws l out-cons)
	(let loop ([l l])
	  (if (null? l)
	      (values null null)
	      (let-values ([(in out) (loop (cdr l))])
		(cond
		 [(and (stx-pair? (car l))
		       (let ([id (stx-car (car l))])
			 (and (identifier? id)
			      (ormap (lambda (k) (module-identifier=? k id)) kws))))
		  (values (cons (car l) in) out)]
		 [else
		  (values in (out-cons (car l) out))])))))

      (define (extract* kws l)
	(let-values ([(in out) (extract kws l void)])
	  in))

      (define (flatten alone l)
	(apply append
	       (map (lambda (i)
		      (let ([l (let ([l (syntax->list i)])
				 (if (ormap (lambda (i)
					      (module-identifier=? (car l) i))
					    (syntax-e (quote-syntax (-init -init-field -field))))
				     (cddr l)
				     (cdr l)))])
			(if alone
			    (map (lambda (i)
				   (if (identifier? i)
				       (alone i)
				       (cons (stx-car i)
					     (stx-car (stx-cdr i)))))
				 l)
			    l)))
		    l)))

      ;; Used with flatten:
      (define (pair i) (cons i i))

      (define (normalize-init/field i)
	;; Put i in ((iid eid) optional-expr) form
	(cond
	 [(identifier? i) (list (list i i))]
	 [else (let ([a (stx-car i)])
		 (if (identifier? a)
		     (cons (list a a) (stx-cdr i))
		     i))]))

      (define (norm-init/field-iid norm) (stx-car (stx-car norm)))
      (define (norm-init/field-eid norm) (stx-car (stx-cdr (stx-car norm))))

      ;; expands an expression enough that we can check whether it has
      ;; the right form for a method; must use local syntax definitions
      (define (proc-shape name orig-stx xform? 
			  the-obj the-finder
			  bad class-name expand-stop-names
			  def-ctx lookup-localize)
	(define (expand expr locals)
	  (local-expand
	   expr
	   'expression
	   (append locals expand-stop-names)
	   def-ctx))
	;; Checks whether the vars sequence is well-formed
	(define (vars-ok? vars)
	  (or (identifier? vars)
	      (stx-null? vars)
	      (and (stx-pair? vars)
		   (identifier? (stx-car vars))
		   (vars-ok? (stx-cdr vars)))))
	;; mk-name: constructs a method name
	;; for error reporting, etc.
	(define (mk-name name)
	  (datum->syntax-object 
	   #f 
	   (string->symbol (format "~a method~a~a" 
				   (syntax-e name)
				   (if class-name
				       " in "
				       "")
				   (or class-name 
				       ""))) 
	   #f))
	;; -- tranform loop starts here --
	(let loop ([stx orig-stx][can-expand? #t][name name][locals null])
	  (syntax-case stx (lambda case-lambda letrec-values let-values)
	    [(lambda vars body1 body ...)
	     (vars-ok? (syntax vars))
	     (if xform?
		 (with-syntax ([the-obj the-obj]
			       [the-finder the-finder]
			       [name (mk-name name)])
		   (let ([l (syntax/loc stx 
			      (lambda (the-obj . vars) 
				(let-syntax ([the-finder (quote-syntax the-obj)])
				  body1 body ...)))])
		     (with-syntax ([l (recertify (add-method-property l) stx)])
		       (syntax/loc stx 
			 (let ([name l]) name)))))
		 stx)]
	    [(lambda . _)
	     (bad "ill-formed lambda expression for method" stx)]
	    [(case-lambda [vars body1 body ...] ...)
	     (andmap vars-ok? (syntax->list (syntax (vars ...))))
	     (if xform?
		 (with-syntax ([the-obj the-obj]
			       [the-finder the-finder]
			       [name (mk-name name)])
		   (let ([cl (syntax/loc stx
			       (case-lambda [(the-obj . vars) 
					     (let-syntax ([the-finder (quote-syntax the-obj)])
					       body1 body ...)] ...))])
		     (with-syntax ([cl (recertify (add-method-property cl) stx)])
		       (syntax/loc stx
			 (let ([name cl]) name)))))
		 stx)]
	    [(case-lambda . _)
	     (bad "ill-formed case-lambda expression for method" stx)]
	    [(let- ([(id) expr] ...) let-body)
	     (and (or (module-identifier=? (syntax let-) 
					   (quote-syntax let-values))
		      (module-identifier=? (syntax let-) 
					   (quote-syntax letrec-values)))
		  (andmap identifier? (syntax->list (syntax (id ...)))))
	     (let* ([letrec? (module-identifier=? (syntax let-) 
						  (quote-syntax letrec-values))]
		    [ids (syntax->list (syntax (id ...)))]
		    [new-ids (if xform?
				 (map
				  (lambda (id)
				    (datum->syntax-object
				     #f
				     (gensym (syntax-e id))))
				  ids)
				 ids)]
		    [body-locals (append ids locals)]
		    [exprs (map (lambda (expr id)
				  (loop expr #t id (if letrec?
						       body-locals
						       locals)))
				(syntax->list (syntax (expr ...)))
				ids)]
		    [body (let ([body (syntax let-body)])
			    (if (identifier? body)
				(ormap (lambda (id new-id)
					 (and (bound-identifier=? body id)
					      new-id))
				       ids new-ids)
				(loop body #t name body-locals)))])
	       (unless body
		 (bad "bad form for method definition" orig-stx))
	       (with-syntax ([(proc ...) exprs]
			     [(new-id ...) new-ids]
			     [mappings
			      (if xform?
				  (map
				   (lambda (old-id new-id)
				     (with-syntax ([old-id old-id]
						   [old-id-localized (lookup-localize (localize old-id))]
						   [new-id new-id]
						   [the-obj the-obj]
						   [the-finder the-finder])
				       (syntax (old-id (make-direct-method-map 
							(quote-syntax the-finder)
							(quote the-obj)
							(quote-syntax old-id)
							(quote-syntax old-id-localized)
							(quote new-id))))))
				   ids new-ids)
				  null)]
			     [body body])
		 (recertify
		  (if xform?
		      (if letrec?
			  (syntax/loc stx (letrec-syntax mappings
					    (let- ([(new-id) proc] ...) 
						  body)))
			  (syntax/loc stx (let- ([(new-id) proc] ...) 
						(letrec-syntax mappings
						  body))))
		      (syntax/loc stx (let- ([(new-id) proc] ...) 
					    body)))
		  stx)))]
	    [_else 
	     (if can-expand?
		 (loop (expand stx locals) #f name locals)
		 (bad "bad form for method definition" orig-stx))])))

      (define (add-method-property l)
	(syntax-property l 'method-arity-error #t))

      (define method-insp (current-code-inspector))

      (define (recertify new old)
	(syntax-recertify new old method-insp #f))

      ;; --------------------------------------------------------------------------------
      ;; Start here:

      (define (main stx trace-flag super-expr deserialize-id-expr name-id interface-exprs defn-and-exprs)
	(let-values ([(this-id) #'this-id]
		     [(the-obj) (datum->syntax-object (quote-syntax here) (gensym 'self))]
		     [(the-finder) (datum->syntax-object (quote-syntax here) (gensym 'find-self))])

	  (let* ([def-ctx (syntax-local-make-definition-context)]
		 [localized-map (make-bound-identifier-mapping)]
		 [any-localized? #f]
		 [localize/set-flag (lambda (id)
				      (let ([id2 (localize id)])
					(unless (eq? id id2)
					  (set! any-localized? #t))
					id2))]
		 [bind-local-id (lambda (id)
				  (let ([l (localize/set-flag id)])
				    (syntax-local-bind-syntaxes (list id) #f def-ctx)
				    (bound-identifier-mapping-put!
				     localized-map
				     id
				     l)))]
		 [lookup-localize (lambda (id)
				    (bound-identifier-mapping-get
				     localized-map
				     id
				     (lambda ()
                                       ;; If internal & external names are distinguished,
                                       ;; we need to fall back to localize:
                                       (localize id))))])
	    
	    ;; ----- Expand definitions -----
	    (let ([defn-and-exprs (expand-all-forms stx defn-and-exprs def-ctx bind-local-id)]
		  [bad (lambda (msg expr)
			 (raise-syntax-error #f msg stx expr))]
		  [class-name (if name-id
				  (syntax-e name-id)
				  (let ([s (syntax-local-infer-name stx)])
				    (if (syntax? s)
					(syntax-e s)
					s)))])
	      
	      ;; ------ Basic syntax checks -----
	      (for-each (lambda (stx)
			  (syntax-case stx (-init init-rest -field -init-field inherit-field
						  private public override augride
						  public-final override-final augment-final
						  pubment overment augment
						  rename-super inherit inherit/super inherit/inner rename-inner
						  inspect)
			    [(form orig idp ...)
			     (and (identifier? (syntax form))
				  (or (module-identifier=? (syntax form) (quote-syntax -init))
				      (module-identifier=? (syntax form) (quote-syntax -init-field))))
			     
			     (let ([form (syntax-e (stx-car (syntax orig)))])
			       (for-each 
				(lambda (idp)
				  (syntax-case idp ()
				    [id (identifier? (syntax id)) 'ok]
				    [((iid eid)) (and (identifier? (syntax iid))
						      (identifier? (syntax eid))) 'ok]
				    [(id expr) (identifier? (syntax id)) 'ok]
				    [((iid eid) expr) (and (identifier? (syntax iid))
							   (identifier? (syntax eid))) 'ok]
				    [else
				     (bad 
				      (format
				       "~a element is not an optionally renamed identifier or identifier-expression pair"
				       form)
				      idp)]))
				(syntax->list (syntax (idp ...)))))]
			    [(inspect expr)
			     'ok]
			    [(inspect . rest)
			     (bad "ill-formed inspect clause" stx)]
			    [(-init orig . rest)
			     (bad "ill-formed init clause" #'orig)]
			    [(init-rest)
			     'ok]
			    [(init-rest rest)
			     (identifier? (syntax rest))
			     'ok]
			    [(init-rest . rest)
			     (bad "ill-formed init-rest clause" stx)]
			    [(-init-field orig . rest)
			     (bad "ill-formed init-field clause" #'orig)]
			    [(-field orig idp ...)
			     (for-each (lambda (idp)
					 (syntax-case idp ()
					   [(id expr) (identifier? (syntax id)) 'ok]
					   [((iid eid) expr) (and (identifier? (syntax iid))
								  (identifier? (syntax eid)))
					    'ok]
					   [else
					    (bad 
					     "field element is not an optionally renamed identifier-expression pair"
					     idp)]))
				       (syntax->list (syntax (idp ...))))]
			    [(-field orig . rest)
			     (bad "ill-formed field clause" #'orig)]
			    [(private id ...)
			     (for-each
			      (lambda (id)
				(unless (identifier? id)
				  (bad "private element is not an identifier" id)))
			      (syntax->list (syntax (id ...))))]
			    [(private . rest)
			     (bad "ill-formed private clause" stx)]
			    [(form idp ...)
			     (and (identifier? (syntax form))
				  (ormap (lambda (f) (module-identifier=? (syntax form) f))
					 (syntax-e (quote-syntax (public
								   override
								   augride
								   public-final
								   override-final
								   augment-final
								   pubment
								   overment
								   augment
								   inherit
                                                                   inherit/super
                                                                   inherit/inner
								   inherit-field)))))
			     (let ([form (syntax-e (syntax form))])
			       (for-each
				(lambda (idp)
				  (syntax-case idp ()
				    [id (identifier? (syntax id)) 'ok]
				    [(iid eid) (and (identifier? (syntax iid)) (identifier? (syntax eid))) 'ok]
				    [else
				     (bad 
				      (format
				       "~a element is not an identifier or pair of identifiers"
				       form)
				      idp)]))
				(syntax->list (syntax (idp ...)))))]
			    [(public . rest)
			     (bad "ill-formed public clause" stx)]
			    [(override . rest)
			     (bad "ill-formed override clause" stx)]
			    [(augride . rest)
			     (bad "ill-formed augride clause" stx)]
			    [(public-final . rest)
			     (bad "ill-formed public-final clause" stx)]
			    [(override-final . rest)
			     (bad "ill-formed override-final clause" stx)]
			    [(augment-final . rest)
			     (bad "ill-formed augment-final clause" stx)]
			    [(pubment . rest)
			     (bad "ill-formed pubment clause" stx)]
			    [(overment . rest)
			     (bad "ill-formed overment clause" stx)]
			    [(augment . rest)
			     (bad "ill-formed augment clause" stx)]
			    [(inherit . rest)
			     (bad "ill-formed inherit clause" stx)]
			    [(inherit/super . rest)
			     (bad "ill-formed inherit/super clause" stx)]
			    [(inherit/inner . rest)
			     (bad "ill-formed inherit/inner clause" stx)]
			    [(inherit-field . rest)
			     (bad "ill-formed inherit-field clause" stx)]
			    [(kw idp ...)
			     (and (identifier? #'kw)
				  (or (module-identifier=? #'rename-super #'kw)
				      (module-identifier=? #'rename-inner #'kw)))
			     (for-each 
			      (lambda (idp)
				(syntax-case idp ()
				  [(iid eid) (and (identifier? (syntax iid)) (identifier? (syntax eid))) 'ok]
				  [else
				   (bad 
				    (format "~a element is not a pair of identifiers" (syntax-e #'kw))
				    idp)]))
			      (syntax->list (syntax (idp ...))))]
			    [(rename-super . rest)
			     (bad "ill-formed rename-super clause" stx)]
			    [(rename-inner . rest)
			     (bad "ill-formed rename-inner clause" stx)]
			    [_ 'ok]))
			defn-and-exprs)
	      
	      ;; ----- Sort body into different categories -----
	      (let*-values ([(decls exprs)
			     (extract (syntax-e (quote-syntax (inherit-field
							       private
							       public
							       override
							       augride
							       public-final
							       override-final
							       augment-final
							       pubment
							       overment
							       augment
							       rename-super
							       inherit
                                                               inherit/super
                                                               inherit/inner
							       rename-inner)))
				      defn-and-exprs
				      cons)]
			    [(inspect-decls exprs)
			     (extract (list (quote-syntax inspect))
				      exprs
				      cons)]
			    [(plain-inits)
			     ;; Normalize after, but keep un-normal for error reporting
			     (flatten #f (extract* (syntax-e 
						    (quote-syntax (-init init-rest)))
						   exprs))]
			    [(normal-plain-inits) (map normalize-init/field plain-inits)]
			    [(init-rest-decls _)
			     (extract (list (quote-syntax init-rest))
				      exprs
				      void)]
			    [(inits)
			     (flatten #f (extract* (syntax-e 
						    (quote-syntax (-init -init-field)))
						   exprs))]
			    [(normal-inits)
			     (map normalize-init/field inits)]
			    [(plain-fields)
			     (flatten #f (extract* (list (quote-syntax -field)) exprs))]
			    [(normal-plain-fields)
			     (map normalize-init/field plain-fields)]
			    [(plain-init-fields)
			     (flatten #f (extract* (list (quote-syntax -init-field)) exprs))]
			    [(normal-plain-init-fields)
			     (map normalize-init/field plain-init-fields)]
			    [(inherit-fields)
			     (flatten pair (extract* (list (quote-syntax inherit-field)) decls))]
			    [(privates)
			     (flatten pair (extract* (list (quote-syntax private)) decls))]
			    [(publics)
			     (flatten pair (extract* (list (quote-syntax public)) decls))]
			    [(overrides)
			     (flatten pair (extract* (list (quote-syntax override)) decls))]
			    [(augrides)
			     (flatten pair (extract* (list (quote-syntax augride)) decls))]
			    [(public-finals)
			     (flatten pair (extract* (list (quote-syntax public-final)) decls))]
			    [(override-finals)
			     (flatten pair (extract* (list (quote-syntax override-final)) decls))]
			    [(pubments)
			     (flatten pair (extract* (list (quote-syntax pubment)) decls))]
			    [(overments)
			     (flatten pair (extract* (list (quote-syntax overment)) decls))]
			    [(augments)
			     (flatten pair (extract* (list (quote-syntax augment)) decls))]
			    [(augment-finals)
			     (flatten pair (extract* (list (quote-syntax augment-final)) decls))]
			    [(rename-supers)
			     (flatten pair (extract* (list (quote-syntax rename-super)) decls))]
			    [(inherits)
			     (flatten pair (extract* (list (quote-syntax inherit)) decls))]
			    [(inherit/supers)
			     (flatten pair (extract* (list (quote-syntax inherit/super)) decls))]
			    [(inherit/inners)
			     (flatten pair (extract* (list (quote-syntax inherit/inner)) decls))]
			    [(rename-inners)
			     (flatten pair (extract* (list (quote-syntax rename-inner)) decls))])
		
		;; At most one inspect:
		(unless (or (null? inspect-decls)
			    (null? (cdr inspect-decls)))
		  (bad "multiple inspect clauses" (cadr inspect-decls)))
		
		;; At most one init-rest:
		(unless (or (null? init-rest-decls)
			    (null? (cdr init-rest-decls)))
		  (bad "multiple init-rest clauses" (cadr init-rest-decls)))
		
		;; Make sure init-rest is last
		(unless (null? init-rest-decls)
		  (let loop ([l exprs] [saw-rest? #f])
		    (unless (null? l)
		      (cond
		       [(and (stx-pair? (car l))
			     (identifier? (stx-car (car l))))
			(let ([form (stx-car (car l))])
			  (cond
			   [(module-identifier=? #'init-rest form)
			    (loop (cdr l) #t)]
			   [(not saw-rest?) (loop (cdr l) #f)]
			   [(module-identifier=? #'-init form)
			    (bad "init clause follows init-rest clause" (stx-car (stx-cdr (car l))))]
			   [(module-identifier=? #'-init-field form)
			    (bad "init-field clause follows init-rest clause" (stx-car (stx-cdr (car l))))]
			   [else (loop (cdr l) #t)]))]
		       [else (loop (cdr l) saw-rest?)]))))
		
		;; --- Check initialization on inits: ---
		(let loop ([inits inits] [normal-inits normal-inits])
		  (unless (null? normal-inits)
		    (if (stx-null? (stx-cdr (car normal-inits)))
			(loop (cdr inits)(cdr normal-inits))
			(let loop ([inits (cdr inits)] [normal-inits (cdr normal-inits)])
			  (unless (null? inits)
			    (if (stx-null? (stx-cdr (car normal-inits)))
				(bad "initializer without default follows an initializer with default"
				     (car inits))
				(loop (cdr inits) (cdr normal-inits))))))))
		
		;; ----- Extract method definitions; check that they look like procs -----
		;;  Optionally transform them, can expand even if not transforming.
		(let* ([field-names (map norm-init/field-iid
					 (append normal-plain-fields normal-plain-init-fields))]
		       [inherit-field-names (map car inherit-fields)]
		       [plain-init-names (map norm-init/field-iid normal-plain-inits)]
		       [inherit-names (map car inherits)]
		       [inherit/super-names (map car inherit/supers)]
		       [inherit/inner-names (map car inherit/inners)]
		       [rename-super-names (map car rename-supers)]
		       [rename-inner-names (map car rename-inners)]
		       [local-public-dynamic-names (map car (append publics overrides augrides
								    overments augments
								    override-finals augment-finals))]
		       [local-public-names (append (map car (append pubments public-finals))
						   local-public-dynamic-names)]
		       [local-method-names (append (map car privates) local-public-names)]
		       [expand-stop-names (append
					   local-method-names
					   field-names
					   inherit-field-names
					   plain-init-names
					   inherit-names
                                           inherit/super-names
                                           inherit/inner-names
					   rename-super-names
					   rename-inner-names
					   (kernel-form-identifier-list
					    (quote-syntax here)))])
		  ;; Do the extraction:
		  (let-values ([(methods          ; (listof (cons id stx))
				 private-methods  ; (listof (cons id stx))
				 exprs            ; (listof stx)
				 stx-defines)     ; (listof (cons (listof id) stx))
				(let loop ([exprs exprs][ms null][pms null][es null][sd null])
				  (if (null? exprs)
				      (values (reverse! ms) (reverse! pms) (reverse! es) (reverse! sd))
				      (syntax-case (car exprs) (define-values define-syntaxes)
					[(define-values (id ...) expr)
					 (let ([ids (syntax->list (syntax (id ...)))])
					   ;; Check form:
					   (for-each (lambda (id)
						       (unless (identifier? id)
							 (bad "not an identifier for definition" id)))
						     ids)
					   ;; method defn? (id in the list of privates/publics/overrides/augrides?)
					   (if (ormap (lambda (id)
							(ormap (lambda (i) (bound-identifier=? i id))
							       local-method-names))
						      ids)
					       ;; Yes, it's a method:
					       (begin
						 (unless (null? (cdr ids))
						   (bad "each method variable needs its own definition"
							(car exprs)))
						 (let ([expr (proc-shape #f (syntax expr) #f 
									 the-obj the-finder
									 bad class-name expand-stop-names
									 def-ctx lookup-localize)]
						       [public? (ormap (lambda (i) 
									 (bound-identifier=? i (car ids)))
								       local-public-names)])
						   (loop (cdr exprs) 
							 (if public?
							     (cons (cons (car ids) expr) ms)
							     ms)
							 (if public?
							     pms
							     (cons (cons (car ids) expr) pms))
							 es
							 sd)))
					       ;; Non-method defn:
					       (loop (cdr exprs) ms pms (cons (car exprs) es) sd)))]
					[(define-values . _)
					 (bad "ill-formed definition" (car exprs))]
					[(define-syntaxes (id ...) expr)
					 (let ([ids (syntax->list (syntax (id ...)))])
					   (for-each (lambda (id) (unless (identifier? id)
								    (bad "syntax name is not an identifier" id)))
						     ids)
					   (loop (cdr exprs) ms pms es (cons (cons ids (car exprs)) sd)))]
					[(define-syntaxes . _)
					 (bad "ill-formed syntax definition" (car exprs))]
					[_else
					 (loop (cdr exprs) ms pms (cons (car exprs) es) sd)])))])
		    
		    ;; ---- Extract all defined names, including field accessors and mutators ---
		    (let ([defined-syntax-names (apply append (map car stx-defines))]
			  [defined-method-names (append (map car methods)
							(map car private-methods))]
			  [private-field-names (let loop ([l exprs])
						 (if (null? l)
						     null
						     (syntax-case (car l) (define-values)
						       [(define-values (id ...) expr)
							(append (syntax->list (syntax (id ...)))
								(loop (cdr l)))]
						       [_else (loop (cdr l))])))]
			  [init-mode (cond
				      [(null? init-rest-decls) 'normal]
				      [(stx-null? (stx-cdr (car init-rest-decls))) 'stop]
				      [else 'list])])
		      
		      ;; -- Look for duplicates --
		      (let ([dup (check-duplicate-identifier
				  (append defined-syntax-names
					  defined-method-names
					  private-field-names
					  field-names
					  inherit-field-names
					  plain-init-names
					  inherit-names
                                          inherit/super-names
                                          inherit/inner-names
					  rename-super-names
					  rename-inner-names))])
			(when dup
			  (bad "duplicate declared identifier" dup)))
		      
		      ;; -- Could still have duplicates within private/public/override/augride --
		      (let ([dup (check-duplicate-identifier local-method-names)])
			(when dup
			  (bad "duplicate declared identifier" dup)))
		      
		      ;; -- Check for duplicate external method names, init names, or field names
		      (let ([check-dup
			     (lambda (what l)
			       (let ([ht (make-hash-table)])
				 (for-each (lambda (id)
					     (when (hash-table-get ht (syntax-e id) #f)
					       (bad (format "duplicate declared external ~a name" what) id))
					     (hash-table-put! ht (syntax-e id) #t))
					   l)))])
			;; method names
			(check-dup "method" (map cdr (append publics overrides augrides
							     pubments overments augments
							     public-finals override-finals augment-finals)))
			;; inits
			(check-dup "init" (map norm-init/field-eid (append normal-inits)))
			;; fields
			(check-dup "field" (map norm-init/field-eid (append normal-plain-fields normal-plain-init-fields))))
		      
		      ;; -- Check that private/public/override/augride are defined --
		      (let ([ht (make-hash-table)]
			    [stx-ht (make-hash-table)])
			(for-each
			 (lambda (defined-name)
			   (let ([l (hash-table-get ht (syntax-e defined-name) null)])
			     (hash-table-put! ht (syntax-e defined-name) (cons defined-name l))))
			 defined-method-names)
			(for-each
			 (lambda (defined-name)
			   (let ([l (hash-table-get stx-ht (syntax-e defined-name) null)])
			     (hash-table-put! stx-ht (syntax-e defined-name) (cons defined-name l))))
			 defined-syntax-names)
			(for-each
			 (lambda (pubovr-name)
			   (let ([l (hash-table-get ht (syntax-e pubovr-name) null)])
			     (unless (ormap (lambda (i) (bound-identifier=? i pubovr-name)) l)
			       ;; Either undefined or defined as syntax:
			       (let ([stx-l (hash-table-get stx-ht (syntax-e pubovr-name) null)])
				 (if (ormap (lambda (i) (bound-identifier=? i pubovr-name)) stx-l)
				     (bad 
				      "method declared but defined as syntax"
				      pubovr-name)
				     (bad 
				      "method declared but not defined"
				      pubovr-name))))))
			 local-method-names))

		      ;; ---- Check that rename-inner doesn't have a non-final decl ---
		      (unless (null? rename-inners)
			(let ([ht (make-hash-table)])
			  (for-each (lambda (pub)
				      (hash-table-put! ht (syntax-e (cdr pub)) #t))
				    (append publics public-finals overrides override-finals augrides))
			  (for-each (lambda (inn)
				      (when (hash-table-get ht (syntax-e (cdr inn)) #f)
					(bad
					 "inner method is locally declared as public, override, public-final, override-final, or augride"
					 (cdr inn))))
				    rename-inners)))
		      
		      ;; ---- Convert expressions ----
		      ;;  Non-method definitions to set!
		      ;;  Initializations args access/set!
		      (let ([exprs (map (lambda (e)
					  (syntax-case e (define-values -field init-rest)
					    [(define-values (id ...) expr)
					     (syntax/loc e (set!-values (id ...) expr))]
					    [(_init orig idp ...)
					     (and (identifier? (syntax _init))
						  (ormap (lambda (it) 
							   (module-identifier=? it (syntax _init)))
							 (syntax-e (quote-syntax (-init
										  -init-field)))))
					     (let* ([norms (map normalize-init/field
								(syntax->list (syntax (idp ...))))]
						    [iids (map norm-init/field-iid norms)]
						    [exids (map norm-init/field-eid norms)])
					       (with-syntax ([(id ...) iids]
							     [(idpos ...) (map localize/set-flag exids)]
							     [(defval ...) 
							      (map (lambda (norm)
								     (if (stx-null? (stx-cdr norm))
									 (syntax #f)
									 (with-syntax ([defexp (stx-car (stx-cdr norm))])
									   (syntax (lambda () defexp)))))
								   norms)]
							     [class-name class-name])
						 (syntax/loc e 
						   (begin 
						     1 ; to ensure a non-empty body
						     (set! id (extract-arg 'class-name `idpos init-args defval))
						     ...))))]
					    [(-field orig idp ...)
					     (with-syntax ([(((iid eid) expr) ...)
							    (map normalize-init/field (syntax->list #'(idp ...)))])
					       (syntax/loc e (begin 
							       1 ; to ensure a non-empty body
							       (set! iid expr)
							       ...)))]
					    [(init-rest id/rename)
					     (with-syntax ([n (+ (length plain-inits)
								 (length plain-init-fields)
								 -1)]
							   [id (if (identifier? #'id/rename)
								   #'id/rename
								   (stx-car #'id/rename))])
					       (syntax/loc e (set! id (extract-rest-args n init-args))))]
					    [(init-rest)
					     (syntax (void))]
					    [_else e]))
					exprs)]
			    [mk-method-temp
			     (lambda (id-stx)
			       (datum->syntax-object (quote-syntax here)
						     (gensym (syntax-e id-stx))))]
			    [rename-super-extras (append overments overrides override-finals inherit/supers)]
			    [rename-inner-extras (append pubments overments augments inherit/inners)]
			    [all-rename-inners (append (map car rename-inners)
						       (generate-temporaries (map car pubments))
						       (generate-temporaries (map car overments))
						       (generate-temporaries (map car augments))
						       (generate-temporaries (map car inherit/inners)))]
                            [all-inherits (append inherits inherit/supers inherit/inners)]
			    [definify (lambda (l)
					(map bind-local-id l)
					l)])

			;; ---- set up field and method mappings ----
			(with-syntax ([(rename-super-orig ...) (definify (map car rename-supers))]
				      [(rename-super-orig-localized ...) (map lookup-localize (map car rename-supers))]
				      [(rename-super-extra-orig ...) (map car rename-super-extras)]
				      [(rename-super-temp ...) (definify (generate-temporaries (map car rename-supers)))]
				      [(rename-super-extra-temp ...) (generate-temporaries (map car rename-super-extras))]
				      [(rename-inner-orig ...) (definify (map car rename-inners))]
				      [(rename-inner-orig-localized ...) (map lookup-localize (map car rename-inners))]
				      [(rename-inner-extra-orig ...) (map car rename-inner-extras)]
				      [(rename-inner-temp ...) (generate-temporaries (map car rename-inners))]
				      [(rename-inner-extra-temp ...) (generate-temporaries (map car rename-inner-extras))]
				      [(private-name ...) (map car privates)]
				      [(private-name-localized ...) (map lookup-localize (map car privates))]
				      [(private-temp ...) (map mk-method-temp (map car privates))]
				      [(pubment-name ...) (map car pubments)]
				      [(pubment-name-localized ...) (map lookup-localize (map car pubments))]
				      [(pubment-temp ...) (map
							   mk-method-temp
							   (map car pubments))]
				      [(public-final-name ...) (map car public-finals)]
				      [(public-final-name-localized ...) (map lookup-localize (map car public-finals))]
				      [(public-final-temp ...) (map
								mk-method-temp
								(map car public-finals))]
				      [(method-name ...) (append local-public-dynamic-names
								 (map car all-inherits))]
				      [(method-name-localized ...) (map lookup-localize
									(append local-public-dynamic-names
										(map car all-inherits)))]
				      [(method-accessor ...) (generate-temporaries
							      (map car
								   (append publics overrides augrides
									   overments augments
									   override-finals augment-finals
									   all-inherits)))]
				      [(inherit-field-accessor ...) (generate-temporaries
								     (map (lambda (id)
									    (format "get-~a"
										    (syntax-e id)))
									  inherit-field-names))]
				      [(inherit-field-mutator ...) (generate-temporaries
								    (map (lambda (id)
									   (format "set-~a!"
										   (syntax-e id)))
									 inherit-field-names))]
				      [(inherit-name ...) (definify (map car all-inherits))]
				      [(inherit-field-name ...) (definify inherit-field-names)]
				      [(inherit-field-name-localized ...) (map lookup-localize inherit-field-names)]
				      [(local-field ...) (definify
							   (append field-names
								   private-field-names))]
				      [(local-field-localized ...) (map lookup-localize
									(append field-names
										private-field-names))]
				      [(local-field-pos ...) (let loop ([pos 0][l (append field-names
											  private-field-names)])
							       (if (null? l)
								   null
								   (cons pos (loop (add1 pos) (cdr l)))))]
				      [(plain-init-name ...) (definify plain-init-names)]
                                      [(plain-init-name-localized ...) (map lookup-localize plain-init-names)]
				      [(local-plain-init-name ...) (generate-temporaries plain-init-names)])
			  (let ([mappings
				 ;; make-XXX-map is supplied by private/classidmap.ss
				 (with-syntax ([the-obj the-obj]
					       [the-finder the-finder]
					       [this-id this-id]
                                               [trace-flag (if trace-flag (syntax #t) (syntax #f))])
				   (syntax 
				    ([(inherit-field-name ...
							  local-field ...
							  rename-super-orig ...
							  rename-inner-orig ...
							  method-name ...
							  private-name ...
							  public-final-name ...
							  pubment-name ...)
				      (values
				       (make-field-map trace-flag
                                                       (quote-syntax the-finder)
						       (quote the-obj)
						       (quote-syntax inherit-field-name)
						       (quote-syntax inherit-field-name-localized)
						       (quote-syntax inherit-field-accessor)
						       (quote-syntax inherit-field-mutator)
						       '())
				       ...
				       (make-field-map trace-flag
                                                       (quote-syntax the-finder)
						       (quote the-obj)
						       (quote-syntax local-field)
						       (quote-syntax local-field-localized)
						       (quote-syntax local-accessor)
						       (quote-syntax local-mutator)
						       '(local-field-pos))
				       ...
				       (make-rename-super-map (quote-syntax the-finder)
							      (quote the-obj)
							      (quote-syntax rename-super-orig)
							      (quote-syntax rename-super-orig-localized)
							      (quote-syntax rename-super-temp))
				       ...
				       (make-rename-inner-map (quote-syntax the-finder)
							      (quote the-obj)
							      (quote-syntax rename-inner-orig)
							      (quote-syntax rename-inner-orig-localized)
							      (quote-syntax rename-inner-temp))
				       ...
				       (make-method-map (quote-syntax the-finder)
							(quote the-obj)
							(quote-syntax method-name)
							(quote-syntax method-name-localized)
							(quote-syntax method-accessor))
				       ...
				       (make-direct-method-map (quote-syntax the-finder)
							       (quote the-obj)
							       (quote-syntax private-name)
							       (quote-syntax private-name-localized)
							       (quote private-temp))
				       ...
				       (make-direct-method-map (quote-syntax the-finder)
							       (quote the-obj)
							       (quote-syntax public-final-name)
							       (quote-syntax public-final-name-localized)
							       (quote public-final-temp))
				       ...
				       (make-direct-method-map (quote-syntax the-finder)
							       (quote the-obj)
							       (quote-syntax pubment-name)
							       (quote-syntax pubment-name-localized)
							       (quote pubment-temp))
				       ...)])))]
				[extra-init-mappings (syntax 
                                                      ([(plain-init-name ...)
                                                        (values
                                                         (make-init-error-map (quote-syntax plain-init-name-localized))
                                                         ...)]))])
                          
			    (let ([find-method 
				   (lambda (methods)
				     (lambda (name)
				       (ormap 
					(lambda (m)
					  (and (bound-identifier=? (car m) name)
					       (with-syntax ([proc (proc-shape (car m) (cdr m) #t 
									       the-obj the-finder
									       bad class-name expand-stop-names
									       def-ctx lookup-localize)]
							     [extra-init-mappings extra-init-mappings])
						 (syntax
						  (syntax-parameterize 
						   ([super-instantiate-param super-error-map]
						    [super-make-object-param super-error-map]
						    [super-new-param super-error-map])
						   (letrec-syntaxes+values extra-init-mappings ()
						     proc))))))
					methods)))]
				  [lookup-localize-cdr (lambda (p) (lookup-localize (cdr p)))])
			      
			      ;; ---- build final result ----
			      (with-syntax ([public-names (map lookup-localize-cdr publics)]
					    [public-final-names (map lookup-localize-cdr public-finals)]
					    [override-names (map lookup-localize-cdr overrides)]
					    [override-final-names (map lookup-localize-cdr override-finals)]
					    [augride-names (map lookup-localize-cdr augrides)]
					    [pubment-names (map lookup-localize-cdr pubments)]
					    [overment-names (map lookup-localize-cdr overments)]
					    [augment-names (map lookup-localize-cdr augments)]
					    [augment-final-names (map lookup-localize-cdr augment-finals)]
					    [(rename-super-name ...) (map lookup-localize-cdr rename-supers)]
					    [(rename-super-extra-name ...) (map lookup-localize-cdr rename-super-extras)]
					    [(rename-inner-name ...) (map lookup-localize-cdr rename-inners)]
					    [(rename-inner-extra-name ...) (map lookup-localize-cdr rename-inner-extras)]
					    [inherit-names (map lookup-localize-cdr all-inherits)]
					    [num-fields (datum->syntax-object
							 (quote-syntax here)
							 (+ (length private-field-names)
							    (length plain-init-fields)
							    (length plain-fields)))]
					    [field-names (map (lambda (norm)
								(lookup-localize (norm-init/field-eid norm)))
							      (append
							       normal-plain-fields
							       normal-plain-init-fields))]
					    [inherit-field-names (map lookup-localize (map cdr inherit-fields))]
					    [init-names (map (lambda (norm)
                                                               (lookup-localize
                                                                (norm-init/field-eid norm)))
							     normal-inits)]
					    [init-mode init-mode]
					    [(private-method ...) (map (find-method private-methods) (map car privates))]
					    [public-methods (map (find-method methods) (map car publics))]
					    [override-methods (map (find-method methods) (map car (append overments
													  override-finals
													  overrides)))]
					    [augride-methods (map (find-method methods) (map car (append augments
													 augment-finals
													 augrides)))]
					    [(pubment-method ...) (map (find-method methods) (map car pubments))]
					    [(public-final-method ...) (map (find-method methods) (map car public-finals))]
					    [mappings mappings]
					    
					    [exprs exprs]
					    [the-obj the-obj]
					    [the-finder the-finder]
					    [name class-name]
					    [(stx-def ...) (map cdr stx-defines)]
					    [super-expression super-expr]
					    [(interface-expression ...) interface-exprs]
					    [inspector (if (pair? inspect-decls)
							   (stx-car (stx-cdr (car inspect-decls)))
							   #'(current-inspector))]
					    [deserialize-id-expr deserialize-id-expr])

				(quasisyntax/loc stx
				  (let ([superclass super-expression]
					[interfaces (list interface-expression ...)])
				    (compose-class 
				     'name superclass interfaces inspector deserialize-id-expr #,any-localized?
				     ;; Field count:
				     num-fields
				     ;; Field names:
				     `field-names
				     `inherit-field-names
				     ;; Method names:
				     `(rename-super-name ... rename-super-extra-name ...)
				     `(rename-inner-name ... rename-inner-extra-name ...)
				     `pubment-names
				     `public-final-names
				     `public-names
				     `overment-names
				     `override-final-names
				     `override-names
				     `augment-names
				     `augment-final-names
				     `augride-names
				     `inherit-names
				     ;; Init arg names (in order)
				     `init-names
				     (quote init-mode)
				     ;; Methods (when given needed super-methods, etc.):
				     #, ;; Attach srcloc (useful for profiling)
				     (quasisyntax/loc stx
				       (lambda (local-accessor
						local-mutator
						inherit-field-accessor ...  ; inherit
						inherit-field-mutator ...
						rename-super-temp ... rename-super-extra-temp ...
						rename-inner-temp ... rename-inner-extra-temp ...
						method-accessor ...) ; for a local call that needs a dynamic lookup
					 (syntax-parameterize
					  ([this-param (make-this-map (quote-syntax this-id)
								      (quote-syntax the-finder)
								      (quote the-obj))])
					  (let-syntaxes
					   mappings
					   (syntax-parameterize 
					    ([super-param
					      (lambda (stx)
						(syntax-case stx (rename-super-extra-orig ...)
						  [(_ rename-super-extra-orig . args) 
						   (generate-super-call 
						    stx
						    (quote-syntax the-finder)
						    (quote the-obj)
						    (quote-syntax rename-super-extra-temp)
						    (syntax args))]
						  ...
						  [(_ id . args)
						   (identifier? #'id)
						   (raise-syntax-error
						    #f
						    (string-append
						     "identifier for super call does not have an override, "
						     "override-final, overment, or inherit/super declaration")
						    stx
						    #'id)]
						  [_else
						   (raise-syntax-error
						    #f
						    "expected an identifier after the keyword"
						    stx)]))]
					     [inner-param
					      (lambda (stx)
						(syntax-case stx (rename-inner-extra-orig ...)
						  [(_ default-expr rename-inner-extra-orig . args)
						   (generate-inner-call 
						    stx
						    (quote-syntax the-finder)
						    (quote the-obj)
						    (syntax default-expr)
						    (quote-syntax rename-inner-extra-temp)
						    (syntax args))]
						  ...
						  [(_ default-expr id . args)
						   (identifier? #'id)
						   (raise-syntax-error
						    #f
						    (string-append
						     "identifier for inner call does not have a pubment, augment, "
						     "overment, or inherit/inner declaration")
						    stx
						    #'id)]
						  [(_)
						   (raise-syntax-error
						    #f
						    "expected a default-value expression after the keyword"
						    stx
						    #'id)]
						  [_else
						   (raise-syntax-error
						    #f
						    "expected an identifier after the keyword and default-value expression"
						    stx)]))])
					    stx-def ...
					    (letrec ([private-temp private-method]
						     ...
						     [pubment-temp pubment-method]
						     ...
						     [public-final-temp public-final-method]
						     ...)
					      (values
					       (list pubment-temp ... public-final-temp ... . public-methods)
					       (list . override-methods)
					       (list . augride-methods)
					       ;; Initialization
					       #, ;; Attach srcloc (useful for profiling)
					       (quasisyntax/loc stx
						 (lambda (the-obj super-go si_c si_inited? si_leftovers init-args)
						   (let-syntax ([the-finder (quote-syntax the-obj)])
						     (syntax-parameterize
						      ([super-instantiate-param
							(lambda (stx)
							  (syntax-case stx () 
							    [(_ (arg (... ...)) (kw kwarg) (... ...))
							     (with-syntax ([stx stx])
							       (syntax (-instantiate super-go stx (the-obj si_c si_inited? 
													   si_leftovers)
										     (list arg (... ...)) 
										     (kw kwarg) (... ...))))]))]
						       [super-new-param
							(lambda (stx)
							  (syntax-case stx () 
							    [(_ (kw kwarg) (... ...))
							     (with-syntax ([stx stx])
							       (syntax (-instantiate super-go stx (the-obj si_c si_inited? 
													   si_leftovers)
										     null
										     (kw kwarg) (... ...))))]))]
						       [super-make-object-param
							(lambda (stx)
							  (let ([code 
								 (quote-syntax
								  (lambda args
								    (super-go the-obj si_c si_inited? si_leftovers args null)))])
							    (if (identifier? stx)
								code
								(datum->syntax-object
								 code
								 (cons code
								       (cdr (syntax-e stx)))))))])
						      (letrec-syntaxes+values
                                                          ([(plain-init-name) (make-init-redirect 
                                                                               (quote-syntax set!)
                                                                               (quote-syntax #%app)
                                                                               (quote-syntax local-plain-init-name)
                                                                               (quote-syntax plain-init-name-localized))] ...)
                                                          ([(local-plain-init-name) undefined] ...)
                                                          (void) ; in case the body is empty
                                                          . exprs))))))))))))
				     ;; Not primitive:
				     #f))))))))))))))))

      (define (core-class* trace-flag)
        (lambda (stx)
          (syntax-case stx ()
            [(_  super-expression (interface-expr ...)
                 defn-or-expr
                 ...)
             (main stx trace-flag
                   #'super-expression 
                   #f #f
                   (syntax->list #'(interface-expr ...))
                   (syntax->list #'(defn-or-expr ...)))])))

      (define (core-class trace-flag)
        (lambda (stx)
          (syntax-case stx ()
            [(_ super-expression
                defn-or-expr
                ...)
             (main stx trace-flag
                   #'super-expression 
                   #f #f
                   null
                   (syntax->list #'(defn-or-expr ...)))])))

      (define (core-class/derived trace-flag)
        (lambda (stx)
          (syntax-case stx ()
            [(_  orig-stx
                 [name-id super-expression (interface-expr ...) deserialize-id-expr]
                 defn-or-expr
                 ...)
             (main #'orig-stx trace-flag
                   #'super-expression 
                   #'deserialize-id-expr 
                   (and (syntax-e #'name-id) #'name-id)
                   (syntax->list #'(interface-expr ...))
                   (syntax->list #'(defn-or-expr ...)))])))

      ;; The class* and class entry points:
      (values
       ;; class*
       (core-class* #f)
       ;; class
       (core-class #f)
       ;; class/derived
       (core-class/derived #f)
       ;; class*-traced
       (core-class* #t)
       ;; class-traced
       (core-class #t)
       ;; class/derived-traced
       (core-class/derived #t)
       )))

  (define-syntax (-define-serializable-class stx)
    (syntax-case stx ()
      [(_ orig-stx name super-expression (interface-expr ...)
	  defn-or-expr ...)
       (let ([deserialize-name-info (datum->syntax-object
				     #'name
				     (string->symbol
				      (format "deserialize-info:~a" (syntax-e #'name)))
				     #'name)])
	 (unless (memq (syntax-local-context) '(top-level module))
	   (raise-syntax-error
	    #f
	    "allowed only at the top level or within a module top level"
	    #'orig-stx))
	 (with-syntax ([deserialize-name-info deserialize-name-info]
		       [(provision ...) (if (eq? (syntax-local-context) 'module)
					    #`((provide #,deserialize-name-info))
					    #'())])
	   #'(begin
	       (define-values (name deserialize-name-info)
		 (class/derived orig-stx [name
					  super-expression 
					  (interface-expr ...)
					  #'deserialize-name-info]
		   defn-or-expr ...))
	       provision ...)))]))

  (define-syntax (define-serializable-class* stx)
    (syntax-case stx ()
      [(_ name super-expression (interface-expr ...)
	  defn-or-expr ...)
       (with-syntax ([orig-stx stx])
	 #'(-define-serializable-class orig-stx
				       name
				       super-expression
				       (interface-expr ...)
				       defn-or-expr ...))]))

  (define-syntax (define-serializable-class stx)
    (syntax-case stx ()
      [(_ name super-expression
	  defn-or-expr ...)
       (with-syntax ([orig-stx stx])
	 #'(-define-serializable-class orig-stx
				       name
				       super-expression
				       ()
				       defn-or-expr ...))]))
  

  (define-syntaxes (private* public* pubment* override* overment* augride* augment*
			     public-final* override-final* augment-final*)
    (let ([mk
	   (lambda (who decl-form)
	     (lambda (stx)
	       (unless (class-top-level-context? (syntax-local-context))
		 (raise-syntax-error
		  #f
		  "use of a class keyword is not in a class top-level"
		  stx))
	       (syntax-case stx ()
		 [(_ binding ...)
		  (let ([bindings (syntax->list (syntax (binding ...)))])
		    (let ([name-exprs
			   (map (lambda (binding)
				  (syntax-case binding ()
				    [(name expr)
				     (identifier? (syntax name))
				     (cons (syntax name) (syntax expr))]
				    [_else
				     (identifier? (syntax name))
				     (raise-syntax-error
				      #f
				      "expected an identifier and expression"
				      stx
				      binding)]))
				bindings)])
		      (with-syntax ([(name ...) (map car name-exprs)]
				    [(expr ...) (map cdr name-exprs)]
				    [decl-form decl-form])
			(syntax
			 (begin
			   (decl-form name ...)
			   (define name expr)
			   ...)))))])))])
      (values
       (mk 'private* (syntax private))
       (mk 'public* (syntax public))
       (mk 'pubment* (syntax pubment))
       (mk 'override* (syntax override))
       (mk 'overment* (syntax overment))
       (mk 'augride* (syntax augride))
       (mk 'augment* (syntax augment))
       (mk 'public-final* (syntax public-final))
       (mk 'override-final* (syntax override-final))
       (mk 'augment-final* (syntax augment)))))

  (define-syntaxes (define/private define/public define/pubment 
		     define/override define/overment
		     define/augride define/augment
		     define/public-final define/override-final define/augment-final)
    (let ([mk
	   (lambda (decl-form)
	     (lambda (stx)
	       (unless (class-top-level-context? (syntax-local-context))
		 (raise-syntax-error
		  #f
		  "use of a class keyword is not in a class top-level"
		  stx))
	       (let-values ([(id rhs) (normalize-definition stx #'lambda)])
		 (quasisyntax/loc stx
		   (begin
		     (#,decl-form #,id)
		     (define #,id #,rhs))))))])
      (values
       (mk #'private)
       (mk #'public)
       (mk #'pubment)
       (mk #'override)
       (mk #'overment)
       (mk #'augride)
       (mk #'augment)
       (mk #'public-final)
       (mk #'override-final)
       (mk #'augment-final))))

  (define-syntax (define-local-member-name stx)
    (syntax-case stx ()
      [(_ id ...)
       (let ([ids (syntax->list (syntax (id ...)))])
	 (for-each (lambda (id)
		     (unless (identifier? id)
		       (raise-syntax-error
			#f
			"expected an identifier"
			stx
			id)))
		   ids)
	 (let ([dup (check-duplicate-identifier ids)])
	   (when dup
	     (raise-syntax-error
	      #f
	      "duplicate identifier"
	      stx
	      dup)))
	 (if (eq? (syntax-local-context) 'top-level)
	     ;; Does nothing in particular at the top level:
	     (syntax/loc stx (define-syntaxes (id ...) (values 'id ...)))
	     ;; Map names to private indicators, which are made private
	     ;;  simply by introduction:
	     (with-syntax ([(gen-id ...) (generate-temporaries ids)])
	       (with-syntax ([stx-defs
			      ;; Need to attach srcloc to this definition:
			      (syntax/loc stx
				(define-syntaxes (id ...)
				  (values (make-private-name (quote-syntax id) 
							     ((syntax-local-certifier) (quote-syntax gen-id)))
					  ...)))])
		 (syntax/loc stx
		   (begin
		     (define-values (gen-id ...)
		       (values (generate-local-member-name 'id) ...))
		     stx-defs))))))]))

  (define-syntax (define-member-name stx)
    (syntax-case stx ()
      [(_ id expr)
       (let ([name #'id])
	 (unless (identifier? name)
	   (raise-syntax-error
	    #f
	    "expected an identifier for definition"
	    stx
	    name))
	 (with-syntax ([stx-def
			;; Need to attach srcloc to this definition:
			(syntax/loc stx
			  (define-syntax id
			    (make-private-name (quote-syntax id) 
					       ((syntax-local-certifier) (quote-syntax member-name)))))])
	   #'(begin
	       (define member-name (check-member-key 'id expr))
	       stx-def)))]))

  (define (generate-local-member-name id)
    (string->uninterned-symbol
     (symbol->string id)))


  (define-values (struct:member-key make-member-key member-name-key? member-key-ref member-key-set!)
    (make-struct-type 'member-name-key
                      #f
                      1 0 #f
                      (list
                       (cons prop:custom-write 
                             (lambda (v p write?)
                               (fprintf p "#<member-key:~a>" (member-key-id v)))))))

  (define member-key-id (make-struct-field-accessor member-key-ref 0))

  (define (check-member-key id v)
    (unless (member-name-key? v)
      (error 'define-local-member-name "not a member key for ~a: ~e" id v))
    (member-key-id v))

  (define-syntax (member-name-key stx)
    (syntax-case stx ()
      [(_ id)
       (identifier? #'id)
       (with-syntax ([id (localize #'id)])
	 (syntax/loc stx (make-member-key `id)))]
      [(_ x)
       (raise-syntax-error
	#f
	"not an identifier"
	stx
	#'x)]))

  (define (generate-member-key)
    (make-member-key (generate-local-member-name (gensym 'member))))

  (define (member-name-key=? a b)
    (if (and (member-name-key? a)
             (member-name-key? b))
        (eq? (member-key-id a) (member-key-id b))
        (eq? a b)))

  (define (member-name-key-hash-code a)
    (unless (member-name-key? a)
      (raise-type-error
       'member-name-key-hash-code
       "member name key"
       a))
    (eq-hash-code (member-key-id a)))

  ;;--------------------------------------------------------------------
  ;;  class implementation
  ;;--------------------------------------------------------------------

  (define-struct class (name
			pos supers     ; pos is subclass depth, supers is vector
			self-interface ; self interface
			insp-mk        ; dummy struct maker to control inspection access

			method-width   ; total number of methods
			method-ht      ; maps public names to vector positions
			method-ids     ; reverse-ordered list of public method names

			methods        ; vector of methods
			beta-methods   ; vector of vector of methods
			meth-flags     ; vector: #f => primitive-implemented
                                       ;         'final => final

			field-width    ; total number of fields
			field-ht       ; maps public field names to (cons class pos)
			field-ids      ; list of public field names

			struct:object  ; structure type for instances
			object?        ; predicate
			make-object    ; : (-> object), constructor that creates an uninitialized object
			field-ref      ; accessor
			field-set!     ; mutator

			init-args      ; list of symbols in order; #f => only by position
			init-mode      ; 'normal, 'stop (don't accept by-pos for super), or 'list

                        init           ; initializer
                                       ; :   object
                                       ;     (object class (box boolean) leftover-args new-by-pos-args new-named-args 
				       ;      -> void) // always continue-make-super?
                                       ;     class
                                       ;     (box boolean)
                                       ;     leftover-args
                                       ;     named-args
                                       ;  -> void
                        
			serializer     ; proc => serializer, #f => not serializable
			fixup          ; for deserialization

			no-super-init?); #t => no super-init needed
                    insp)
  
  ;; compose-class: produces one result if `deserialize-id' is #f, two
  ;;                results if `deserialize-id' is not #f
  (define (compose-class name                ; symbol
			 super               ; class
			 interfaces          ; list of interfaces
			 inspector           ; inspector or #f
			 deserialize-id      ; identifier or #f
			 any-localized?      ; #t => need to double-check distinct external names

			 num-fields          ; total fields (public & private)
			 public-field-names  ; list of symbols (shorter than num-fields)
			 inherit-field-names ; list of symbols (not included in num-fields)
			 
			 rename-super-names  ; list of symbols
			 rename-inner-names
			 pubment-names
			 public-final-names
			 public-normal-names
			 overment-names
			 override-final-names
			 override-normal-names
			 augment-names
			 augment-final-names
			 augride-normal-names
			 inherit-names

			 init-args           ; list of symbols in order, or #f
			 init-mode           ; 'normal, 'stop, or 'list
			 
			 make-methods        ; takes field and method accessors
			 make-struct:prim)   ; see "primitive classes", below

    ;; -- Check superclass --
    (unless (class? super)
      (obj-error 'class* "superclass expression returned a non-class: ~a~a" 
		 super
		 (for-class name)))

    (when any-localized?
      (check-still-unique name
			  init-args
			  "initialization argument names")
      ;; We intentionally leave inherited names out of the lists below,
      ;;  on the threory that it's ok to decide to inherit from
      ;;  yourself:
      (check-still-unique name
			  (append public-field-names)
			  "field names")
      (check-still-unique name
			  (append pubment-names public-final-names public-normal-names
				  overment-names override-final-names override-normal-names
				  augment-names augment-final-names augride-normal-names)
			  "method names"))

    ;; -- Create new class's name --
    (let* ([name (or name
		     (let ([s (class-name super)])
		       (and s 
			    (not (eq? super object%))
			    (if (symbol? s)
				(format "derived-from-~a" s)
				s))))]
	   ;; Combine method lists
	   [public-names (append pubment-names public-final-names public-normal-names)]
	   [override-names (append overment-names override-final-names override-normal-names)]
	   [augride-names (append augment-names augment-final-names augride-normal-names)]
	   [final-names (append public-final-names override-final-names augment-final-names)]
	   [augonly-names (append pubment-names overment-names augment-names)]
	   ;; Mis utilities
	   [no-new-methods? (null? public-names)]
	   [no-method-changes? (and (null? public-names)
				    (null? override-names)
				    (null? augride-names))]
	   [no-new-fields? (null? public-field-names)]
	   [xappend (lambda (a b) (if (null? b) a (append a b)))])

      ;; -- Check interfaces ---
      (for-each
       (lambda (intf)
	 (unless (interface? intf)
	   (obj-error 'class* "interface expression returned a non-interface: ~a~a" 
		      intf
		      (for-class name))))
       interfaces)

      ;; -- Check inspectors ---
      (when inspector
	(unless (inspector? inspector)
	  (obj-error 'class* "inspect class result is not an inspector or #f: ~a~a" 
		      inspector
		      (for-class name))))

      ;; -- Match method and field names to indices --
      (let ([method-ht (if no-new-methods?
			   (class-method-ht super)
			   (make-hash-table))]
	    [field-ht (if no-new-fields?
			  (class-field-ht super)
			  (make-hash-table))]
	    [super-method-ht (class-method-ht super)]
	    [super-method-ids (class-method-ids super)]
	    [super-field-ids (class-field-ids super)]
	    [super-field-ht (class-field-ht super)])

	;; Put superclass ids in tables, with pos
	(unless no-new-methods?
	  (let loop ([ids super-method-ids][p (sub1 (class-method-width super))])
	    (unless (null? ids)
	      (hash-table-put! method-ht (car ids) p)
	      (loop (cdr ids) (sub1 p)))))
	(unless no-new-fields?
	  (let loop ([ids super-field-ids])
	    (unless (null? ids)
	      (hash-table-put! field-ht (car ids) (hash-table-get super-field-ht (car ids)))
	      (loop (cdr ids)))))

	;; Put new ids in table, with pos (replace field pos with accessor info later)
	(unless no-new-methods?
	  (let loop ([ids public-names][p (class-method-width super)])
	    (unless (null? ids)
	      (when (hash-table-get method-ht (car ids) #f)
		(obj-error 'class* "superclass already contains method: ~a~a" 
			   (car ids)
			   (for-class name)))
	      (hash-table-put! method-ht (car ids) p)
	      (loop (cdr ids) (add1 p)))))
	(unless no-new-fields?
	  (let loop ([ids public-field-names][p (class-field-width super)])
	    (unless (null? ids)
	      (when (hash-table-get field-ht (car ids) #f)
		(obj-error 'class* "superclass already contains field: ~a~a" 
			   (car ids)
			   (for-class name)))
	      (hash-table-put! field-ht (car ids) p)
	      (loop (cdr ids) (add1 p)))))

	;; Check that superclass has expected fields
	(for-each (lambda (id)
		    (unless (hash-table-get field-ht id #f)
		      (obj-error 'class* "superclass does not provide field: ~a~a" 
				 id
				 (for-class name))))
		  inherit-field-names)

	;; Check that superclass has expected methods, and get indices
	(let ([get-indices
	       (lambda (method-ht what ids)
		 (map
		  (lambda (id)
		    (hash-table-get 
		     method-ht id
		     (lambda ()
		       (obj-error 'class* 
				  "~a does not provide an expected method for ~a: ~a~a" 
				  (if (eq? method-ht super-method-ht) "superclass" "class")
				  what
				  id
				  (for-class name)))))
		  ids))]
	      [method-width (+ (class-method-width super) (length public-names))]
	      [field-width (+ (class-field-width super) num-fields)])
	  (let ([inherit-indices (get-indices super-method-ht "inherit" inherit-names)]
		[replace-augonly-indices (get-indices super-method-ht "overment" overment-names)]
		[replace-final-indices (get-indices super-method-ht "override-final" override-final-names)]
		[replace-normal-indices (get-indices super-method-ht "override" override-normal-names)]
		[refine-augonly-indices (get-indices super-method-ht "augment" augment-names)]
		[refine-final-indices (get-indices super-method-ht "augment-final" augment-final-names)]
		[refine-normal-indices (get-indices super-method-ht "augride" augride-normal-names)]
		[rename-super-indices (get-indices super-method-ht "rename-super" rename-super-names)]
		[rename-inner-indices (get-indices method-ht "rename-inner" rename-inner-names)]
		[new-augonly-indices (get-indices method-ht "pubment" pubment-names)]
		[new-final-indices (get-indices method-ht "public-final" public-final-names)]
		[new-normal-indices (get-indices method-ht "public" public-normal-names)])

	    ;; -- Check that all interfaces are satisfied --
	    (for-each
	     (lambda (intf)
	       (for-each
		(lambda (var)
		  (unless (hash-table-get method-ht var #f)
		    (obj-error 'class* 
			       "interface-required method missing: ~a~a~a" 
			       var
			       (for-class name)
			       (for-intf (interface-name intf)))))
		(interface-public-ids intf)))
	     interfaces)
	    (let ([c (get-implement-requirement interfaces 'class* (for-class name))])
	      (when (and c (not (subclass? super c)))
		(obj-error 'class* 
			   "interface-required implementation not satisfied~a~a"
			   (for-class name)
			   (let ([r (class-name c)])
			     (if r
				 (format " required class: ~a" r)
				 "")))))

	    ;; -- For serialization, check that the superclass is compatible --
	    (when deserialize-id
	      (unless (class-serializer super)
		(obj-error 'class*
			   "superclass is not serialiazable, not transparent, and does not implement externalizable<%>: ~e~a"
			   super
			   (for-class name))))

	    ;; ---- Make the class and its interface ----
	    (let* ([class-make (if name
				   (make-naming-constructor 
				    struct:class
				    (string->symbol (format "class:~a" name)))
				   make-class)]
		   [interface-make (if name
				       (make-naming-constructor 
					struct:interface
					(string->symbol (format "interface:~a" name)))
				       make-interface)]
		   [method-names (append (reverse public-names) super-method-ids)]
		   [field-names (append public-field-names super-field-ids)]
		   [super-interfaces (cons (class-self-interface super) interfaces)]
		   [i (interface-make name super-interfaces #f method-names #f)]
		   [methods (if no-method-changes?
				(class-methods super)
				(make-vector method-width))]
		   [beta-methods (if no-method-changes?
				     (class-beta-methods super)
				     (make-vector method-width))]
		   [meth-flags (if no-method-changes?
				   (class-meth-flags super)
				   (make-vector method-width))]
		   [c (class-make name
				  (add1 (class-pos super))
				  (list->vector (append (vector->list (class-supers super)) (list #f)))
				  i
				  (let-values ([(struct: make- ? -ref -set) (make-struct-type 'insp #f 0 0 #f null inspector)])
				    make-)
				  method-width method-ht method-names
				  methods beta-methods meth-flags
				  field-width field-ht field-names
				  'struct:object 'object? 'make-object 'field-ref 'field-set!
				  init-args
				  init-mode
				  'init
				  #f #f ; serializer is set later
				  (and make-struct:prim #t))]
		   [obj-name (if name
				 (string->symbol (format "object:~a" name))
				 'object)]
		   ;; Used only for prim classes
		   [preparer (lambda (name)
			       ;; Map symbol to number:
			       (hash-table-get method-ht name))]
		   [dispatcher (lambda (obj n)
				 ;; Extract method:
				 (vector-ref (class-methods (object-ref obj)) n))])
	      (setup-all-implemented! i)
	      (vector-set! (class-supers c) (add1 (class-pos super)) c)

	      ;; --- Make the new object struct ---
	      (let*-values ([(prim-object-make prim-object? struct:prim-object)
			     (if make-struct:prim
				 (make-struct:prim c prop:object preparer dispatcher)
				 (values #f #f #f))]
			    [(struct:object object-make object? object-field-ref object-field-set!)
			     (if make-struct:prim
				 ;; Use prim struct:
				 (values struct:prim-object prim-object-make prim-object? #f #f)
				 ;; Normal struct creation:
				 (make-struct-type obj-name
						   (class-struct:object super)
						   0 ;; No init fields
						   ;; Fields for new slots:
						   num-fields undefined
						   ;; Map object property to class:
						   (append
						    (list (cons prop:object c))
						    (if deserialize-id
							(list
							 (cons prop:serializable
							       ;; Serialization:
							       (make-serialize-info
								(lambda (obj) 
								  ((class-serializer c) obj))
								deserialize-id
								(and (not inspector)
								     (not (interface-extension? i externalizable<%>))
								     (eq? #t (class-serializer super)))
								(or (current-load-relative-directory) 
								    (current-directory)))))
							null))
						   inspector))])
		(set-class-struct:object! c struct:object)
		(set-class-object?! c object?)
		(set-class-make-object! c object-make)
		(unless (zero? num-fields)
		  ;; We need these only if there are fields, used for for public-field
		  ;; access or for inspection:
		  (set-class-field-ref! c object-field-ref)
		  (set-class-field-set!! c object-field-set!))

		;; --- Build field accessors and mutators ---
		;;  Use public field names to name the accessors and mutators
		(let-values ([(inh-accessors inh-mutators)
			      (values
			       (map (lambda (id) (make-class-field-accessor super id))
				    inherit-field-names)
			       (map (lambda (id) (make-class-field-mutator super id))
				    inherit-field-names))])
		  ;; -- Reset field table to register accessor and mutator info --
		  ;;  There are more accessors and mutators than public fields...
		  (let loop ([ids public-field-names][pos 0])
		    (unless (null? ids)
		      (hash-table-put! field-ht (car ids) (cons c pos))
		      (loop (cdr ids) (add1 pos))))

		  ;; -- Extract superclass methods and make rename-inners ---
		  (let ([rename-supers (map (lambda (index mname)
					(let ([vec (vector-ref (class-beta-methods super) index)])
					  (if (positive? (vector-length vec))
					      (or (vector-ref vec (sub1 (vector-length vec)))
						  (obj-error 'class* 
							     (string-append
							      "superclass method for override, overment, inherit/super, "
							      "or rename-super is not overrideable: ~a~a")
							     mname
							     (for-class name)))
					      (vector-ref (class-methods super) index))))
				      rename-super-indices
				      rename-super-names)]
			[rename-inners (let ([new-augonly (make-vector method-width #f)])
					 (define (get-depth index)
					   (+ (if (index . < . (class-method-width super))
						  (vector-length (vector-ref (class-beta-methods super) 
									     index))
						  0)
					      (if (vector-ref new-augonly index) 0 -1)))
					 ;; To compute `rename-inner' indices, we need to know which methods
					 ;;  are augonly in this new class.
					 (for-each (lambda (id)
						     (vector-set! new-augonly (hash-table-get method-ht id) #t))
						   (append pubment-names overment-names))
					 (for-each (lambda (mname index)
						     (let ([depth (get-depth index)])
						       (when (negative? depth)
							 (obj-error 'class* 
								    (string-append
								     "superclass method for augride, augment, inherit/inner, "
								     "or rename-inner method is not augmentable: ~a~a")
								    mname
								    (for-class name)))))
						   (append augride-normal-names
							   augment-final-names
							   rename-inner-names)
						   (append (get-indices method-ht "augride" augride-normal-names)
							   refine-final-indices
							   rename-inner-indices))
					 ;; Now that checking is done, add `augment':
					 (for-each (lambda (id)
						     (vector-set! new-augonly (hash-table-get method-ht id) #t))
						   augment-names)
					 (map (lambda (mname index)
						(let ([depth (get-depth index)])
						  (lambda (obj)
						    (vector-ref (vector-ref (class-beta-methods (object-ref obj)) 
									    index)
								depth))))
					      rename-inner-names
					      rename-inner-indices))])
		    ;; -- Create method accessors --
		    (let ([method-accessors (map (lambda (index)
						   (lambda (obj)
						     (vector-ref (class-methods (object-ref obj)) index)))
						 (append new-normal-indices replace-normal-indices refine-normal-indices
							 replace-augonly-indices refine-augonly-indices
							 replace-final-indices refine-final-indices
							 inherit-indices))])
		      
		      ;; -- Get new methods and initializers --
		      (let-values ([(new-methods override-methods augride-methods init)
				    (apply make-methods
					   object-field-ref
					   object-field-set!
					   (append inh-accessors
						   inh-mutators
						   rename-supers
						   rename-inners
						   method-accessors))])
			;; -- Fill in method tables --
			;;  First copy old methods
			(unless no-method-changes?
			  (hash-table-for-each
			   super-method-ht
			   (lambda (name index)
			     (vector-set! methods index (vector-ref (class-methods super) index))
			     (vector-set! beta-methods index (vector-ref (class-beta-methods super) index))
			     (vector-set! meth-flags index (vector-ref (class-meth-flags super) index)))))
			;; Add new methods:
			(for-each (lambda (index method)
				    (vector-set! methods index method)
				    (vector-set! beta-methods index (vector)))
				  (append new-augonly-indices new-final-indices new-normal-indices)
				  new-methods)
			;; Override old methods:
			(for-each (lambda (index method id)
				    (when (eq? 'final (vector-ref meth-flags index))
				      (obj-error 'class* 
						 "cannot override or augment final method: ~a~a"
						 id
						 (for-class name)))
				    (let ([v (vector-ref beta-methods index)])
				      (if (zero? (vector-length v))
					  ;; Normal mode - set vtable entry
					  (vector-set! methods index method)
					  ;; Under final mode - set extended vtable entry
					  (let ([v (list->vector (vector->list v))])
					    (vector-set! v (sub1 (vector-length v)) method)
					    (vector-set! beta-methods index v))))
				    (vector-set! meth-flags index (not make-struct:prim)))
				  (append replace-augonly-indices replace-final-indices replace-normal-indices
					  refine-augonly-indices refine-final-indices refine-normal-indices)
				  (append override-methods augride-methods)
				  (append override-names augride-names))
			;; Expand `rename-inner' vector, adding a #f to indicate that
			;;  no rename-inner function is available, so far
			(for-each (lambda (id)
				    (let ([index (hash-table-get method-ht id)])
				      (let ([v (list->vector (append (vector->list (vector-ref beta-methods index))
								     (list #f)))])
					(vector-set! beta-methods index v))))
				  augonly-names)
			;; Mark final methods:
			(for-each (lambda (id)
				    (let ([index (hash-table-get method-ht id)])
				      (vector-set! meth-flags index 'final)))
				  final-names)
			
			;; --- Install serialize info into class --
			(set-class-serializer!
			 c
			 (cond
			  [(interface-extension? i externalizable<%>)
			   (let ([index (car (get-indices method-ht "???" '(externalize)))])
			     (lambda (obj)
			       (vector ((vector-ref methods index) obj))))]
			  [(and (or deserialize-id
				    (not inspector))
				(class-serializer super))
			   => (lambda (ss)
				(lambda (obj)
				  (vector (cons (ss obj)
						(let loop ([i 0])
						  (if (= i num-fields)
						      null
						      (cons (object-field-ref obj i)
							    (loop (add1 i)))))))))]
			  [else #f]))

			(set-class-fixup!
			 c
			 ;; Used only for non-externalizable:
			 (lambda (o args)
			   (if (pair? args)
			       (begin
				 ((class-fixup super) o (vector-ref (car args) 0))
				 (let loop ([i 0][args (cdr args)])
				   (unless (= i num-fields)
				     (object-field-set! o i (car args))
				     (loop (add1 i) (cdr args)))))
			       (begin
				 ((class-fixup super) o args)
				 (let loop ([i 0])
				   (unless (= i num-fields)
				     (object-field-set! o i (object-field-ref args i))
				     (loop (add1 i))))))))

			;; --- Install initializer into class ---
			(set-class-init! c init)
				    
			;; -- result is the class, and maybe deserialize-info ---
			(if deserialize-id
			    (values c (make-deserialize-info
				       (if (interface-extension? i externalizable<%>)
					   (lambda (args)
					     (let ([o (make-object c)])
					       (send o internalize args)
					       o))
					   (lambda (args)
					     (let ([o (object-make)])
					       ((class-fixup c) o args)
					       o)))
				       (if (interface-extension? i externalizable<%>)
					   (lambda ()
					     (error 'deserialize "cannot deserialize instance with cycles~a"
						    (for-class name)))
					   (lambda ()
					     (let ([o (object-make)])
					       (values o
						       (lambda (o2)
							 ((class-fixup c) o o2))))))))
			    c))))))))))))

  (define (check-still-unique name syms what)
    (let ([ht (make-hash-table)])
      (for-each (lambda (s)
		  (when (hash-table-get ht s 
					(lambda ()
					  (hash-table-put! ht s #t)
					  #f))
		    (obj-error 'class* "external ~a mapped to overlapping keys~a" 
			       what
			       (for-class name))))
		syms)))

  (define-values (prop:object object? object-ref) (make-struct-type-property 'object))

  ;;--------------------------------------------------------------------
  ;;  interfaces
  ;;--------------------------------------------------------------------
  
  ;; >> Simplistic implementation for now <<

  (define-syntax _interface
    (lambda (stx)
      (syntax-case stx ()
	[(_ (interface-expr ...) var ...)
	 (let ([vars (syntax->list (syntax (var ...)))]
	       [name (syntax-local-infer-name stx)])
	   (for-each
	    (lambda (v)
	      (unless (identifier? v)
		(raise-syntax-error #f
				    "not an identifier"
				    stx
				    v)))
	    vars)
	   (let ([dup (check-duplicate-identifier vars)])
	     (when dup
	       (raise-syntax-error #f
				   "duplicate name"
				   stx
				   dup)))
	   (with-syntax ([name (datum->syntax-object #f name #f)]
			 [(var ...) (map localize vars)])
	     (syntax/loc
	      stx
	      (compose-interface
	       'name
	       (list interface-expr ...)
	       `(var ...)))))])))

  (define-struct interface 
                 (name            ; symbol
                  supers          ; (listof interface)
		  all-implemented ; hash-table: interface -> #t
                  public-ids      ; (listof symbol) (in any order?!?)
                  class)          ; (union #f class) -- means that anything implementing
                                  ; this interface must be derived from this class
                 insp)

  (define (compose-interface name supers vars)
    (for-each
     (lambda (intf)
       (unless (interface? intf)
	 (obj-error 'interface 
		    "superinterface expression returned a non-interface: ~a~a" 
		    intf
		    (for-intf name))))
     supers)
    (let ([ht (make-hash-table)])
      (for-each
       (lambda (var)
	 (hash-table-put! ht var #t))
       vars)
      ;; Check that vars don't already exist in supers:
      (for-each
       (lambda (super)
	 (for-each
	  (lambda (var)
	    (when (hash-table-get ht var #f)
	      (obj-error 'interface "variable already in superinterface: ~a~a~a" 
			 var
			 (for-intf name)
			 (let ([r (interface-name super)])
			   (if r
			       (format " already in: ~a" r)
			       "")))))
	  (interface-public-ids super)))
       supers)
      ;; Check for [conflicting] implementation requirements
      (let ([class (get-implement-requirement supers 'interface (for-intf name))]
	    [interface-make (if name
				(make-naming-constructor 
				 struct:interface
				 (string->symbol (format "interface:~a" name)))
				make-interface)])
	;; Add supervars to table:
	(for-each
	 (lambda (super)
	   (for-each
	    (lambda (var) (hash-table-put! ht var #t))
	    (interface-public-ids super)))
	 supers)
	;; Done
	(let ([i (interface-make name supers #f (hash-table-map ht (lambda (k v) k)) class)])
	  (setup-all-implemented! i)
	  i))))

  ;; setup-all-implemented! : interface -> void
  ;;  Creates the hash table for all implemented interfaces
  (define (setup-all-implemented! i)
    (let ([ht (make-hash-table)])
      (hash-table-put! ht i #t)
      (for-each (lambda (si)
		  (hash-table-for-each
		   (interface-all-implemented si)
		   (lambda (k v)
		     (hash-table-put! ht k #t))))
		(interface-supers i))
      (set-interface-all-implemented! i ht)))

  (define (get-implement-requirement interfaces where for)
    (let loop ([class #f]
	       [supers interfaces])
      (if (null? supers)
	  class
	  (let ([c (interface-class (car supers))])
	    (loop
	     (cond
	      [(not c) class]
	      [(not class) c]
	      [(subclass? c class) class]
	      [(subclass? class c) c]
	      [else
	       (obj-error 
		where
		"conflicting class implementation requirements in superinterfaces~a"
		for)])
	     (cdr supers))))))
  
  ;;--------------------------------------------------------------------
  ;;  object%
  ;;--------------------------------------------------------------------
  
  (define (make-naming-constructor type name)
    (let-values ([(struct: make- ? -accessor -mutator)
		  (make-struct-type name type 0 0 #f null insp)])
      make-))
  
  (define object<%> ((make-naming-constructor struct:interface 'interface:object%)
		     'object% null #f null #f))
  (setup-all-implemented! object<%>)
  (define object% ((make-naming-constructor struct:class 'class:object%)
		   'object%
		   0 (vector #f) 
		   object<%>
		   void ; never inspectable

		   0 (make-hash-table) null
		   (vector) (vector) (vector)
		   
		   0 (make-hash-table) null
		   
		   'struct:object object? 'make-object
		   'field-ref-not-needed 'field-set!-not-needed

		   null
		   'normal

		   (lambda (this super-init si_c si_inited? si_leftovers args) 
		     (unless (null? args)
		       (unused-args-error this args))
		     (void))

		   (lambda (obj) #(()))        ; serialize
		   (lambda (obj args) (void))  ; deserialize-fixup

		   #t)) ; no super-init

  (vector-set! (class-supers object%) 0 object%)
  (let*-values ([(struct:obj make-obj obj? -get -set!)
                 (make-struct-type 'object #f 0 0 #f (list (cons prop:object object%)) #f)])
    (set-class-struct:object! object% struct:obj)
    (set-class-make-object! object% make-obj))
  (set-class-object?! object% object?) ; don't use struct pred; it wouldn't work with prim classes

  (set-interface-class! object<%> object%)

  ;;--------------------------------------------------------------------
  ;;  instantiation
  ;;--------------------------------------------------------------------
  
  (define-syntaxes (new new-traced)

    (let* ([core-new
            (lambda (instantiate-stx stx)
              (syntax-case stx ()
                [(_ cls (id arg) ...)
                 (andmap identifier? (syntax->list (syntax (id ...))))
                 (quasisyntax/loc stx
                   ((unsyntax instantiate-stx) cls () (id arg) ...))]
                [(_ cls (id arg) ...)
                 (for-each (lambda (id)
                             (unless (identifier? id)
                               (raise-syntax-error 'new "expected identifier" stx id)))
                           (syntax->list (syntax (id ...))))]
                [(_ cls pr ...)
                 (for-each
                  (lambda (pr)
                    (syntax-case pr ()
                      [(x y) (void)]
                      [else (raise-syntax-error 'new "expected name and value binding" stx pr)]))
                  (syntax->list (syntax (pr ...))))]))])

      (values
       (lambda (stx) (core-new (syntax/loc stx instantiate) stx))
       (lambda (stx) (core-new (syntax/loc stx instantiate-traced) stx)))))
  
  (define make-object 
    (lambda (class . args)
      (do-make-object class args null)))

  (define make-object-traced
    (lambda (class . args)
      (do-make-object-traced class args null)))
  
  (define-syntaxes (instantiate instantiate-traced)

    (let* ([core-instantiate
            (lambda (do-make-object-stx stx)
              (syntax-case stx ()
                [(form class (arg ...) . x)
                 (with-syntax ([orig-stx stx])
                   (quasisyntax/loc stx 
                     (-instantiate (unsyntax do-make-object-stx)
                                   orig-stx (class) (list arg ...) . x)))]))])

      (values
       (lambda (stx)
         (core-instantiate (syntax/loc stx do-make-object) stx))
       (lambda (stx)
         (core-instantiate (syntax/loc stx do-make-object-traced) stx)))))

  ;; Helper; used by instantiate and super-instantiate
  (define-syntax -instantiate
    (lambda (stx)
      (syntax-case stx ()
	[(_ do-make-object orig-stx (maker-arg ...) args (kw arg) ...)
	 (andmap identifier? (syntax->list (syntax (kw ...))))
	 (with-syntax ([(kw ...) (map localize (syntax->list (syntax (kw ...))))])
	   (syntax/loc stx
	     (do-make-object maker-arg ...
			     args
			     (list (cons `kw arg)
				   ...))))]
	[(_ super-make-object orig-stx (make-arg ...) args kwarg ...)
	 ;; some kwarg must be bad:
	 (for-each (lambda (kwarg)
		     (syntax-case kwarg ()
		       [(kw arg)
			(identifier? (syntax kw))
			'ok]
		       [(kw arg)
			(raise-syntax-error
			 #f
			 "by-name argument does not start with an identifier"
			 (syntax orig-stx)
			 kwarg)]
		       [_else
			(raise-syntax-error
			 #f
			 "ill-formed by-name argument"
			 (syntax orig-stx)
			 kwarg)]))
		   (syntax->list (syntax (kwarg ...))))])))

  (define (alist->sexp alist)
    (map (lambda (pair) (list (car pair) (cdr pair))) alist))

  (define-traced (do-make-object class by-pos-args named-args)
    (unless (class? class)
      (raise-type-error 'instantiate "class" class))
    (let ([o ((class-make-object class))])
      (trace-begin
       ;; Initialize it:
       (trace (new-event class o (alist->sexp (get-field-alist o))))
       (trace (initialize-call-event
               o (string->symbol "(constructor)")
               (cons (alist->sexp named-args) by-pos-args)))
       (continue-make-object o class by-pos-args named-args #t)
       (trace (finalize-call-event o))
       o)))

  (define (get-field-alist obj)
    (map (lambda (id) (cons id (get-field/proc id obj)))
         (field-names obj)))

  (define (continue-make-object o c by-pos-args named-args explict-named-args?)
    (let ([by-pos-only? (not (class-init-args c))])
      ;; When a superclass has #f for init-args (meaning "by-pos args with no names"),
      ;; some propagated named args may have #f keys; move them to by-position args.
      (let-values ([(by-pos-args named-args)
                    (if by-pos-only?
                        (let ([l (filter (lambda (x) (not (car x))) named-args)])
                          (if (pair? l)
                              (values (append by-pos-args (map cdr l))
                                      (filter car named-args))
                              (values by-pos-args named-args)))
                        (values by-pos-args named-args))])
        ;; Primitive class with by-pos arguments?
        (when by-pos-only?
          (unless (null? named-args)
            (if explict-named-args?
                (obj-error 
                 'instantiate
                 "class has only by-position initializers, but given by-name arguments:~a~a" 
                 (make-named-arg-string named-args)
                 (for-class (class-name c)))
                ;; If args were implicit from subclass, should report as unused:
                (unused-args-error o named-args))))
        ;; Merge by-pos into named args:
        (let* ([named-args (if (not by-pos-only?)
                               ;; Normal merge
                               (do-merge by-pos-args (class-init-args c) c named-args by-pos-args c)
                               ;; Non-merge for by-position initializers
                               by-pos-args)]
               [leftovers (if (not by-pos-only?)
                              (get-leftovers named-args (class-init-args c))
                              null)])
          ;; In 'list mode, make sure no by-name arguments are left over
          (when (eq? 'list (class-init-mode c))
            (unless (or (null? leftovers)
                        (not (ormap car leftovers)))
              (unused-args-error o (filter car leftovers))))
          (unless (and (eq? c object%)
                       (null? named-args))
            (let ([inited? (box (class-no-super-init? c))])
              ;; ----- Execute the class body -----
              ((class-init c)
               o 
               continue-make-super
               c inited? leftovers ; merely passed through to continue-make-super
               named-args)
              (unless (unbox inited?)
                (obj-error 'instantiate "superclass initialization not invoked by initialization~a"
                           (for-class (class-name c))))))))))

  (define (continue-make-super o c inited? leftovers by-pos-args new-named-args)
    (when (unbox inited?)
      (obj-error 'instantiate "superclass already initialized by class initialization~a"
		 (for-class (class-name c))))
    (set-box! inited? #t)
    (let ([named-args (if (eq? 'list (class-init-mode c))
			  ;; all old args must have been used up
			  new-named-args
			  ;; Normal mode: merge leftover keyword-based args with new ones
			  (append
			   new-named-args
			   leftovers))])
      (continue-make-object o
			    (vector-ref (class-supers c) (sub1 (class-pos c))) 
			    by-pos-args 
			    named-args
			    (pair? new-named-args))))

  (define (do-merge al nl ic named-args by-pos-args c)
    (cond
     [(null? al) named-args]
     [(null? nl)
      ;; continue mapping with superclass init args, if allowed
      (let ([super (and (eq? 'normal (class-init-mode ic))
			(positive? (class-pos ic))
			(vector-ref (class-supers ic) (sub1 (class-pos ic))))])
	(cond
	 [super 
          (if (class-init-args super)
              (do-merge al (class-init-args super) super named-args by-pos-args c)
              ;; Like 'list mode:
              (append (map (lambda (x) (cons #f x)) al)
                      named-args))]
	 [(eq? 'list (class-init-mode ic))
	  ;; All unconsumed named-args must have #f
	  ;;  "name"s, otherwise an error is raised in
	  ;;  the leftovers checking.
	  (append (map (lambda (x) (cons #f x)) al)
		  named-args)]
	 [else
	  (obj-error 'instantiate 
		     "too many initialization arguments:~a~a" 
		     (make-pos-arg-string by-pos-args)
		     (for-class (class-name c)))]))]
     [else (cons (cons (car nl) (car al))
		 (do-merge (cdr al) (cdr nl) ic named-args by-pos-args c))]))

  (define (get-leftovers l names)
    (cond
     [(null? l) null]
     [(memq (caar l) names)
      (get-leftovers (cdr l) (remq (caar l) names))]
     [else (cons (car l) (get-leftovers (cdr l) names))]))

  (define (extract-arg class-name name arguments default)
    (if (symbol? name)
	;; Normal mode
	(let ([a (assq name arguments)])
	  (cond
	   [a (cdr a)]
	   [default (default)]
	   [else (missing-argument-error class-name name)]))
	;; By-position mode
	(cond
	 [(< name (length arguments))
	  (cdr (list-ref arguments name))]
	 [default (default)]
	 [else (obj-error 'instantiate "too few initialization arguments")])))

  (define (extract-rest-args skip arguments)
    (if (< skip (length arguments))
	(map cdr (list-tail arguments skip))
	null))

  (define (make-pos-arg-string args)
    (let ([len (length args)])
      (apply string-append
	     (map (lambda (a)
		    (format " ~e" a))
		  args))))

  (define (make-named-arg-string args)
    (let loop ([args args][count 0])
      (cond
       [(null? args) ""]
       [(= count 3) " ..."]
       [else (let ([rest (loop (cdr args) (add1 count))])
	       (format " (~a ~e)~a"
		       (caar args)
		       (cdar args)
		       rest))])))

  (define (unused-args-error this args)
    (let ([arg-string (make-named-arg-string args)])
      (obj-error 'instantiate "unused initialization arguments:~a~a" 
		 arg-string
		 (for-class/which "instantiated" (class-name (object-ref this))))))

  (define (missing-argument-error class-name name)
    (obj-error 'instantiate "no argument for required init variable: ~a~a"
	       name
	       (if class-name (format " in class: ~a" class-name) "")))

  ;;--------------------------------------------------------------------
  ;;  methods and fields
  ;;--------------------------------------------------------------------
  
  (define-syntaxes (send send/apply send-traced send/apply-traced)
    (let ()

      (define (do-method traced? stx form obj name args rest-arg?)
        (with-syntax ([(sym method receiver)
                       (generate-temporaries (syntax (1 2 3)))])
          (quasisyntax/loc stx
            (let*-values ([(sym) (quasiquote (unsyntax (localize name)))]
                          [(method receiver)
                           (find-method/who '(unsyntax form)
                                            (unsyntax obj)
                                            sym)])
              (unsyntax
               (make-method-call
                traced?
                stx
                (syntax/loc stx receiver)
                (syntax/loc stx unwrap-object)
                (syntax/loc stx method)
                (syntax/loc stx sym)
                args
                rest-arg?))))))

      (define (core-send traced? apply?)
        (lambda (stx)
          (syntax-case stx ()
            [(form obj name . args)
             (identifier? (syntax name))
             (if (stx-list? (syntax args))
                 ;; (send obj name arg ...) or (send/apply obj name arg ...)
                 (do-method traced? stx #'form #'obj #'name #'args apply?)
                 (if apply?
                     ;; (send/apply obj name arg ... . rest)
                     (raise-syntax-error
                      #f "bad syntax (illegal use of `.')" stx)
                     ;; (send obj name arg ... . rest)
                     (do-method traced? stx #'form #'obj #'name
                                (flatten-args #'args) #t)))]
            [(form obj name . args)
             (raise-syntax-error
              #f "method name is not an identifier" stx #'name)])))

      (values
       ;; send
       (core-send #f #f)
       ;; send/apply
       (core-send #f #t)
       ;; send-traced
       (core-send #t #f)
       ;; send/apply-traced
       (core-send #t #t))))
  
  (define-syntaxes (send* send*-traced)
    (let* ([core-send*
            (lambda (traced?)
              (lambda (stx)
                (syntax-case stx ()
                  [(form obj clause ...)
                   (quasisyntax/loc stx
                     (let* ([o obj])
                       (unsyntax-splicing
                        (map
                         (lambda (clause-stx)
                           (syntax-case clause-stx ()
                             [(meth . args)
                              (quasisyntax/loc stx
                                ((unsyntax (if traced?
                                               (syntax/loc stx send-traced)
                                               (syntax/loc stx send)))
                                 o meth . args))]
                             [_ (raise-syntax-error
                                 #f "bad method call" stx clause-stx)]))
                         (syntax->list (syntax (clause ...)))))))])))])
      (values (core-send* #f) (core-send* #t))))
    
  ;; find-method/who : symbol[top-level-form/proc-name]
  ;;                   any[object] 
  ;;                   symbol[method-name] 
  ;;               -> (values method-proc object)
  ;; returns the method's procedure and a function to unwrap `this' in the case
  ;; that this is a wrapper object that is just "falling thru".
  (define (find-method/who who in-object name)
    (unless (object? in-object)
      (obj-error who "target is not an object: ~e for method: ~a"
		 in-object name))
    
    (let-syntax ([loop-body
                  (lambda (stx)
                    (syntax-case stx ()
                      [(_ abs-object wrapper-case)
                       (identifier? (syntax abs-object))
                       (syntax
                        (let* ([c (object-ref abs-object)]
                               [pos (hash-table-get (class-method-ht c) name #f)])
                          (cond
                            [pos (values (vector-ref (class-methods c) pos) abs-object)]
                            [(wrapper-object? abs-object) wrapper-case]
                            [else
                             (obj-error who "no such method: ~a~a"
                                        name
                                        (for-class (class-name c)))])))]))])
      (loop-body
       in-object
       (let loop ([loop-object in-object])
         (loop-body
          loop-object
          (loop (wrapper-object-wrapped loop-object)))))))
  

  (define (class-field-X who which cwhich class name)
    (unless (class? class)
      (raise-type-error who "class" class))
    (unless (symbol? name)
      (raise-type-error who "symbol" name))
    (let ([p (hash-table-get (class-field-ht class) name
			     (lambda ()
			       (obj-error who "no such field: ~a~a"
					  name
					  (for-class (class-name class)))))])
      (which (cwhich (car p)) (cdr p))))
  
  (define (make-class-field-accessor class name)
    (class-field-X 'class-field-accessor
		   make-struct-field-accessor class-field-ref
		   class name))
  
  (define (make-class-field-mutator class name)
    (class-field-X 'class-field-mutator 
		   make-struct-field-mutator class-field-set!
		   class name))

  (define-struct generic (name applicable))

  ;; Internally, make-generic comes from the struct def.
  ;; Externally, make-generic is the following procedure.
  ;; The extra `let' gives it the right name.
  (define make-generic/proc
    (let ([make-generic
	   (lambda (class name)
	     (unless (or (class? class) (interface? class))
	       (raise-type-error 'make-generic "class or interface" class))
	     (unless (symbol? name)
	       (raise-type-error 'make-generic "symbol" name))
	     (make-generic
              name
	      (if (interface? class)
		  (let ([intf class])
		    (unless (method-in-interface? name intf)
		      (obj-error 'make-generic "no such method: ~a~a"
				 name
				 (for-intf (interface-name intf))))
		    (lambda (obj)
		      (unless (is-a? obj intf)
			(raise-type-error 
			 (string->symbol (format "generic:~a~a" name (for-intf (interface-name intf))))
			 (format "instance~a" (for-intf (interface-name intf)))
			 obj))
		      (let-values ([(mth ths) (find-method/who 'make-generic obj name)])
                        mth)))
		  (let* ([pos (hash-table-get (class-method-ht class) name
                                              (lambda ()
                                                (obj-error 'make-generic "no such method: ~a~a"
                                                           name
                                                           (for-class (class-name class)))))]
                         [instance? (class-object? class)]
                         [dynamic-generic
                          (lambda (obj)
                            (unless (instance? obj)
                              (raise-type-error 
                               (string->symbol (format "generic:~a~a" name (for-class (class-name class))))
                               (format "instance~a" (for-class (class-name class)))
                               obj))
                            (vector-ref (class-methods (object-ref obj)) pos))])
                    (if (eq? 'final (vector-ref (class-meth-flags class) pos))
                        (let ([method (vector-ref (class-methods class) pos)])
                          (lambda (obj)
                            (unless (instance? obj)
                              (dynamic-generic obj))
                            method))
                        dynamic-generic)))))])
      make-generic))

  (define-syntaxes (send-generic send-generic-traced)
    (let ()
      (define (core-send-generic traced?)
        (lambda (stx)
          (syntax-case stx ()
            [(_ object generic . args)
             (let* ([args-stx (syntax args)]
                    [proper? (stx-list? args-stx)]
                    [flat-stx (if proper? args-stx (flatten-args args-stx))])
               (with-syntax ([(gen obj)
                              (generate-temporaries (syntax (generic object)))])
                 (quasisyntax/loc stx
                   (let* ([obj object]
                          [gen generic])
                     (unsyntax
                      (make-method-call
                       traced?
                       stx
                       (syntax obj)
                       (syntax/loc stx unwrap-object)
                       (syntax/loc stx ((generic-applicable gen) obj))
                       (syntax/loc stx (generic-name gen))
                       flat-stx
                       (not proper?)))))))])))
      (values (core-send-generic #f) (core-send-generic #t))))

  (define-syntaxes (class-field-accessor class-field-mutator generic/form)
    (let ([mk
	   (lambda (make targets)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ class-expr name)
		  (let ([name (syntax name)])
		    (unless (identifier? name)
		      (raise-syntax-error
		       #f
		       "expected an indentifier"
		       stx
		       name))
		    (with-syntax ([name (localize name)]
				  [make make])
		      (syntax/loc stx (make class-expr `name))))]
		 [(_ class-expr)
		  (raise-syntax-error
		   #f
		   (format "expected a field name after the ~a expression"
			   targets)
		   stx)])))])
      (values
       (mk (quote-syntax make-class-field-accessor) "class")
       (mk (quote-syntax make-class-field-mutator) "class")
       (mk (quote-syntax make-generic/proc) "class or interface"))))

  (define-syntax (class-field-accessor-traced stx)
    (syntax-case stx ()
      [(form class name)
       (syntax/loc stx
         (let* ([accessor (class-field-accessor class name)])
           (lambda (obj)
             (begin0 (accessor obj)
                     (get-event obj 'name)))))]))

  (define-syntax (class-field-mutator-traced stx)
    (syntax-case stx ()
      [(form class name)
       (syntax/loc stx
         (let* ([mutator (class-field-mutator class name)])
           (lambda (obj value)
             (begin0 (mutator obj value)
                     (set-event obj 'name value)))))]))

  (define-syntaxes (get-field get-field-traced)
    (let ()
      (define (core-get-field traced?)
        (lambda (stx)
          (syntax-case stx ()
            [(_ name obj)
             (identifier? (syntax name))
             (with-syntax ([get (if traced?
                                    (syntax get-field/proc-traced)
                                    (syntax get-field/proc))]
                           [localized (localize (syntax name))])
               (syntax (get `localized obj)))]
            [(_ name obj)
             (raise-syntax-error
              'get-field "expected a field name as first argument"
              stx (syntax name))])))
      (values (core-get-field #f) (core-get-field #t))))
  
  (define-traced (get-field/proc id obj)
    (unless (object? obj)
      (raise-mismatch-error 
       'get-field
       "expected an object, got "
       obj))
    (trace-begin
     (trace (get-event obj id))
     (let loop ([obj obj])
       (let* ([cls (object-ref obj)]
              [field-ht (class-field-ht cls)]
              [index (hash-table-get 
                      field-ht
                      id
                      #f)])
         (cond
          [index
           ((class-field-ref (car index)) obj (cdr index))]
          [(wrapper-object? obj)
           (loop (wrapper-object-wrapped obj))]
          [else
           (raise-mismatch-error 
            'get-field
            (format "expected an object that has a field named ~s, got " id)
            obj)])))))
  
  (define-syntaxes (field-bound? field-bound?-traced)
    (let ()
      (define (core-field-bound? traced?)
        (lambda (stx)
          (syntax-case stx ()
            [(_ name obj)
             (identifier? (syntax name))
             (with-syntax ([localized (localize (syntax name))]
                           [bound? (if traced?
                                       (syntax field-bound?/proc-traced)
                                       (syntax field-bound?/proc))])
               (syntax (bound? `localized obj)))]
            [(_ name obj)
             (raise-syntax-error
              'field-bound? "expected a field name as first argument"
              stx (syntax name))])))
      (values (core-field-bound? #f) (core-field-bound? #t))))
  
  (define-traced (field-bound?/proc id obj)
    (unless (object? obj)
      (raise-mismatch-error 
       'field-bound?
       "expected an object, got "
       obj))
    (trace-begin
     (trace (inspect-event obj))
     (let loop ([obj obj])
       (let* ([cls (object-ref obj)]
              [field-ht (class-field-ht cls)])
         (or (and (hash-table-get field-ht id #f)
                  #t) ;; ensure that only #t and #f leak out, not bindings in ht
             (and (wrapper-object? obj)
                  (loop (wrapper-object-wrapped obj))))))))
  
  (define-traced (field-names obj)
    (unless (object? obj)
      (raise-mismatch-error 
       'field-names
       "expected an object, got "
       obj))
    (trace-begin
     (trace (inspect-event obj))
     (let loop ([obj obj])
       (let* ([cls (object-ref obj)]
              [field-ht (class-field-ht cls)]
              [flds (filter interned? (hash-table-map field-ht (lambda (x y) x)))])
         (if (wrapper-object? obj)
             (append flds (loop (wrapper-object-wrapped obj)))
             flds)))))
    
  (define-syntaxes (with-method with-method-traced)
    (let ()
      (define (core-with-method traced?)
        (lambda (stx)
          (syntax-case stx ()
            [(_ ([id (obj-expr name)] ...) body0 body1 ...)
             (let ([ids (syntax->list (syntax (id ...)))]
                   [names (syntax->list (syntax (name ...)))])
               (for-each (lambda (id name)
                           (unless (identifier? id)
                             (raise-syntax-error #f
                                                 "not an identifier for binding"
                                                 stx
                                                 id))
                           (unless (identifier? name)
                             (raise-syntax-error #f
                                                 "not an identifier for method name"
                                                 stx
                                                 name)))
                         ids names)
               (with-syntax ([(method ...) (generate-temporaries ids)]
                             [(method-obj ...) (generate-temporaries ids)]
                             [(name ...) (map localize names)]
                             [trace-flag (if traced? (syntax/loc stx #t) (syntax/loc stx #f))])
                 (syntax/loc stx (let-values ([(method method-obj)
                                               (let ([obj obj-expr])
                                                 (find-method/who 'with-method obj `name))]
                                              ...)
                                   (letrec-syntaxes+values ([(id) (make-with-method-map
                                                                   trace-flag
                                                                   (quote-syntax set!)
                                                                   (quote-syntax id)
                                                                   (quote-syntax method)
                                                                   (quote-syntax method-obj)
                                                                   (syntax unwrap-object))]
                                                            ...)
                                                           ()
                                                           body0 body1 ...)))))]
            ;; Error cases:
            [(_ (clause ...) . body)
             (begin
               (for-each (lambda (clause)
                           (syntax-case clause ()
                             [(id (obj-expr name))
                              (and (identifier? (syntax id))
                                   (identifier? (syntax name)))
                              'ok]
                             [_else
                              (raise-syntax-error 
                               #f
                               "binding clause is not of the form (identifier (object-expr method-identifier))"
                               stx
                               clause)]))
                         (syntax->list (syntax (clause ...))))
               ;; If we get here, the body must be bad
               (if (stx-null? (syntax body))
                   (raise-syntax-error 
                    #f
                    "empty body"
                    stx)
                   (raise-syntax-error 
                    #f
                    "bad syntax (illegal use of `.')"
                    stx)))]
            [(_ x . rest)
             (raise-syntax-error 
              #f
              "not a binding sequence"
              stx
              (syntax x))])))

      (values
       ;; with-method
       (core-with-method #f)
       ;; with-method-traced
       (core-with-method #t))))
        
  
  ;;--------------------------------------------------------------------
  ;;  class, interface, and object properties
  ;;--------------------------------------------------------------------
  
  (define-traced (is-a? v c)
    (trace-begin
     (trace (when (object? v)
              (inspect-event v)))
     (cond
      [(class? c) ((class-object? c) (unwrap-object v))]
      [(interface? c)
       (and (object? v)
            (implementation? (object-ref (unwrap-object v)) c))]
      [else (raise-type-error 'is-a? "class or interface" 1 v c)])))
  
  (define (subclass? v c)
    (unless (class? c)
      (raise-type-error 'subclass? "class" 1 v c))
    (and (class? v)
	 (let ([p (class-pos c)])
	   (and (<= p (class-pos v))
		(eq? c (vector-ref (class-supers v) p))))))

  (define-traced (object-interface o)
    (unless (object? o)
      (raise-type-error 'object-interface "object" o))
    (trace-begin
     (trace (inspect-event o))
     (class-self-interface (object-ref (unwrap-object o)))))

  (define-traced (object-method-arity-includes? o name cnt)
    (unless (object? o)
      (raise-type-error 'object-method-arity-includes? "object" o))
    (unless (symbol? name)
      (raise-type-error 'object-method-arity-includes? "symbol" name))
    (unless (and (integer? cnt)
		 (exact? cnt)
		 (not (negative? cnt)))
      (raise-type-error 'object-method-arity-includes? "non-negative exact integer" cnt))
    (trace-begin
     (trace (inspect-event o))
     (let loop ([o o])
       (let* ([c (object-ref o)]
              [pos (hash-table-get (class-method-ht c) name #f)])
         (cond
          [pos (procedure-arity-includes? (vector-ref (class-methods c) pos) 
                                          (add1 cnt))]
          [(wrapper-object? o) (loop (wrapper-object-wrapped o))]
          [else #f])))))
  
  (define (implementation? v i)
    (unless (interface? i)
      (raise-type-error 'implementation? "interface" 1 v i))
    (and (class? v)
	 (interface-extension? (class-self-interface v) i)))

  (define (interface-extension? v i)
    (unless (interface? i)
      (raise-type-error 'interface-extension? "interface" 1 v i))
    (and (interface? i)
	 (hash-table-get (interface-all-implemented v) i #f)))
  
  (define (method-in-interface? s i)
    (unless (symbol? s)
      (raise-type-error 'method-in-interface? "symbol" 0 s i))
    (unless (interface? i)
      (raise-type-error 'method-in-interface? "interface" 1 s i))
    (and (memq s (interface-public-ids i)) #t))

  (define (class->interface c)
    (unless (class? c)
      (raise-type-error 'class->interface "class" c))
    (class-self-interface c))
  
  (define (interned? sym)
    (eq? sym (string->symbol (symbol->string sym))))

  (define (interface->method-names i)
    (unless (interface? i)
      (raise-type-error 'interface->method-names "interface" i))
    ;; copy list, and also filter private (interned) methods:
    (apply list-immutable (filter interned? (interface-public-ids i))))

  
  (define-traced (object-info o)
    (unless (object? o)
      (raise-type-error 'object-info "object" o))
    (trace-begin
     (trace (inspect-event o))
     (let loop ([c (object-ref (unwrap-object o))]
                [skipped? #f])
       (if (struct? ((class-insp-mk c)))
           ;; current inspector can inspect this object
           (values c skipped?)
           (if (zero? (class-pos c))
               (values #f #t)
               (loop (vector-ref (class-supers c) (sub1 (class-pos c))) #t))))))

  (define (to-sym s)
    (if (string? s)
        (string->symbol s)
        s))

  (define (class-info c)
    (unless (class? c)
      (raise-type-error 'class-info "class" c))
    (if (struct? ((class-insp-mk c)))
	(let ([super (vector-ref (class-supers c) (sub1 (class-pos c)))])
	  (let loop ([next super][skipped? #f])
	    (if (or (not next)
		    (struct? ((class-insp-mk next))))
		(values (to-sym (class-name c))
			(- (class-field-width c) (class-field-width super))
			(apply list-immutable (filter interned? (class-field-ids c)))
			(class-field-ref c)
			(class-field-set! c)
			next
			skipped?)
		(if (zero? (class-pos next))
		    (loop #f #t)
		    (loop (vector-ref (class-supers next) (sub1 (class-pos next))) #t)))))
	(raise-mismatch-error 'class-info "current inspector cannot inspect class: " c)))

  (define-traced object->vector
    (opt-lambda (in-o [opaque-v '...])
      (unless (object? in-o)
	(raise-type-error 'object->vector "object" in-o))
      (trace-begin
       (trace (inspect-event in-o))
       (let ([o (unwrap-object in-o)])
         (list->vector
          (cons
           (string->symbol (format "object:~a" (class-name (object-ref o))))
           (reverse
            (let-values ([(c skipped?) (object-info o)])
              (let loop ([c c][skipped? skipped?])
                (cond
                 [(not c) (if skipped? (list opaque-v) null)]
                 [else (let-values ([(name num-fields field-ids field-ref
                                           field-set next next-skipped?)
                                     (class-info c)])
                         (let ([rest (loop next next-skipped?)]
                               [here (let loop ([n num-fields])
                                       (if (zero? n)
                                           null
                                           (cons (field-ref o (sub1 n))
                                                 (loop (sub1 n)))))])
                           (append (if skipped? (list opaque-v) null)
                                   here
                                   rest)))]))))))))))
  
  (define (object=? o1 o2)
    (unless (object? o1)
      (raise-type-error 'object=? "object" o1))
    (unless (object? o2)
      (raise-type-error 'object=? "object" o2))
    (eq? (unwrap-object o1)
         (unwrap-object o2)))
    
  ;;--------------------------------------------------------------------
  ;;  primitive classes
  ;;--------------------------------------------------------------------
  
  (define (make-primitive-class 
	   make-struct:prim     ; see below
	   prim-init            ; primitive initializer: takes obj and list of name-arg pairs
	   name                 ; symbol
	   super                ; superclass
	   init-arg-names       ; #f or list of syms and sym--value lists
	   override-names       ; overridden method names
	   new-names            ; new (public) method names
	   override-methods     ; list of methods
	   new-methods)         ; list of methods

    ; The `make-struct:prim' function takes prop:object, a
    ;  class, a preparer, and a dispatcher function, and produces:
    ;    * a struct constructor (must have prop:object)
    ;    * a struct predicate
    ;    * a struct type for derived classes (mustn't have prop:object)
    ;
    ; The supplied preparer takes a symbol and returns a num.
    ; 
    ; The supplied dispatcher takes an object and a num and returns a method.
    ;
    ; When a primitive class has a superclass, the struct:prim maker
    ;  is responsible for ensuring that the returned struct items match
    ;  the supertype predicate.

    (compose-class name
		   (or super object%)
		   null
		   #f
		   #f
		   #f
		   
		   0 null null ; no fields

		   null ; no rename-supers
		   null ; no rename-inners
		   null null new-names
		   null null override-names
		   null null null ; no augrides
		   null ; no inherits
		   
		   ; #f => init args by position only
		   ; sym => required arg
		   ; sym--value list => optional arg
		   (and init-arg-names  
			(map (lambda (s)
			       (if (symbol? s) s (car s)))
			     init-arg-names))
                   'stop
		   
		   (lambda ignored
		     (values
		      new-methods
		      override-methods
		      null ; no augride-methods
		      (lambda (this super-go/ignored si_c/ignored si_inited?/ignored si_leftovers/ignored init-args)
			(apply prim-init this 
			       (if init-arg-names
				   (extract-primitive-args this name init-arg-names init-args)
				   init-args)))))
		   
		   make-struct:prim))

  (define (extract-primitive-args this class-name init-arg-names init-args)
    (let loop ([names init-arg-names][args init-args])
      (cond
       [(null? names)
	(unless (null? args)
	  (unused-args-error this args))
	null]
       [else (let* ([name (car names)]
		    [id (if (symbol? name)
			    name
			    (car name))])
	       (let ([arg (assq id args)])
		 (cond
		  [arg 
		   (cons (cdr arg) (loop (cdr names) (remq arg args)))]
		  [(symbol? name)
		   (missing-argument-error class-name name)]
		  [else
		   (cons (cadr name) (loop (cdr names) args))])))])))

  ;;--------------------------------------------------------------------
  ;;  wrapper for contracts
  ;;--------------------------------------------------------------------

  (define-struct wrapper-field (name ctc-stx))
  (define-struct wrapper-method (name mth-stx))

  (define-values (wrapper-object? wrapper-object-wrapped set-wrapper-object-wrapped! struct:wrapper-object)
    (let-values ([(struct:wrapper-object make-wrapper-object wrapper-object? ref set!)
                  (make-struct-type 'raw-wrapper-object
                                    #f
                                    0
                                    1)])
      (values wrapper-object?
              (lambda (v) (ref v 0))
              (lambda (o v) (set! o 0 v))
              struct:wrapper-object)))
  
  ;; unwrap-object : (union wrapper-object object) -> object
  (define (unwrap-object o)
    (let loop ([o o])
      (if (wrapper-object? o)
          (loop (wrapper-object-wrapped o))
          o)))
  
  ;; make-wrapper-class :   symbol
  ;;                        (listof symbol)
  ;;                        (listof (selector -> method-func-spec[object args -> result]))
  ;;                        (listof symbol)
  ;;                     -> class
  ;; the resulting class is the "proxy" class for the contracted version of an
  ;; object with contracts on the method-ids. 
  
  ;; Overall, objects of this class have one field for the original object,
  ;; one field per method in the contract and one field per field in the contract.
  ;; Each of the methods (passed in) just accesses the initial (method) fields
  ;; (which contain procedures) and calls them and returns their results.
  ;; Those fields do not show up from outside of this file, via the usual
  ;; field accessors. In addition, the class has one field per field that
  ;; will contain the contracted versions of the input fields.
  ;; The class accepts one initialization argument per method and
  ;; one init arg per field (in that order) using the make-object style
  ;; initialization.
  (define (make-wrapper-class class-name method-ids methods field-ids)
    (let* ([supers (vector object% #f)]
           [method-ht (make-hash-table)]
           [method-count (length method-ids)]
           [methods-vec (make-vector method-count #f)]
           
           [field-ht (make-hash-table)]
           [field-count (length field-ids)]
           
           [cls
            (make-class class-name
                        1
                        supers
                        'bogus-self-interface
                        void  ; nothing can be inspected
			
                        method-count
                        method-ht
                        (reverse method-ids)
                        
                        methods-vec
                        (list->vector (map (lambda (x) 'final) method-ids))
			'dont-use-me!
                        
                        (+ 1 field-count method-count)
                        field-ht
                        field-ids
                        
                        #f; struct:object
                        #f; object?
                        #f; make-object ;; -> void
                        #f; field-ref
                        #f; field-set!
                        
                        #f ;; only by position arguments
                        'normal ; init-mode - ??
                        
                        #f ; init
			#f #f ; not serializable
                        #f)])
      (let-values ([(struct:object make-object object? field-ref field-set!)
                    (make-struct-type 'wrapper-object
                                      struct:wrapper-object
                                      0
                                      (+ (length field-ids) (length method-ids))
                                      undefined
                                      (list (cons prop:object cls))
                                      insp)])
        (set-class-struct:object! cls struct:object)
        (set-class-object?! cls object?)
        (set-class-make-object! cls make-object)
        (set-class-field-ref! cls field-ref)
        (set-class-field-set!! cls field-set!)

        (let ([init
               (lambda (o continue-make-super c inited? named-args leftover-args)
                 ;; leftover args will contain:
                 ;; the original object,
                 ;; all of the contract-ized versions of the methods,
                 ;; and all of the contract-ized versions of the fields
                 ;; just fill them into `o'.
                 (set-wrapper-object-wrapped! o (car leftover-args))
                 (let loop ([leftover-args (cdr leftover-args)]
                            [i 0])
                   (unless (null? leftover-args)
                     (field-set! o i (car leftover-args))
                     (loop (cdr leftover-args)
                           (+ i 1))))
                 (continue-make-super o c inited? '() '() '()))])
          (set-class-init! cls init))
        
        ;; fill in the methods vector
        (let loop ([i 0]
                   [methods methods])
          (when (< i method-count)
            (vector-set! methods-vec i ((car methods) field-ref))
            (loop (+ i 1)
                  (cdr methods))))
        
        ;; fill in the methods-ht
        (let loop ([i 0]
                   [method-ids method-ids])
          (when (< i method-count)
            (hash-table-put! method-ht (car method-ids) i)
            (loop (+ i 1)
                  (cdr method-ids))))
        
        ;; fill in the fields-ht
        (let loop ([i 0]
                   [field-ids field-ids])
          (when (< i field-count)
            (hash-table-put! field-ht (car field-ids) (cons cls (+ i method-count)))
            (loop (+ i 1)
                  (cdr field-ids))))
        
        ;; fill in the supers vector
        (vector-set! supers 1 cls)
        
        cls)))
  
  ; extract-vtable : object -> (vectorof method-proc[this args ... -> res])
  (define (extract-vtable o) (class-methods (object-ref o)))

  ; extract-method-ht : object -> hash-table[sym -> number]
  (define (extract-method-ht o) (class-method-ht (object-ref o)))

  ;;--------------------------------------------------------------------
  ;;  misc utils
  ;;--------------------------------------------------------------------
  
  (define undefined (letrec ([x x]) x))

  (define-struct (exn:fail:object exn:fail) () insp)

  (define (obj-error where . msg)
    (raise (make-exn:fail:object
            (string-append (format "~a: " where) (apply format msg))
            (current-continuation-marks))))

  (define (for-class name)
    (if name (format " for class: ~a" name) ""))
  (define (for-class/which which name)
    (if name (format " for ~a class: ~a" which name) ""))
  (define (for-intf name)
    (if name (format " for interface: ~a" name) ""))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; mixin
  ;;

  (define (check-mixin-super mixin-name super% from-ids)
    (let ([mixin-name (or mixin-name 'mixin)])
      (unless (class? super%)
	(error mixin-name "argument is not a class: ~e" super%))
      (for-each (lambda (from-id)
		  (unless (implementation? super% from-id)
		    (error mixin-name "argument does not implement ~e: ~e" from-id super%)))
		from-ids)))

  (define (check-mixin-from-interfaces all-from)
    (for-each (lambda (from-id)
		(unless (interface? from-id)
		  (error 'mixin
			 "expected from-interface, got: ~e; others ~e"
			 from-id
			 all-from)))
	      all-from))

  (define (check-mixin-to-interfaces all-to)
    (for-each (lambda (to-id)
		(unless (interface? to-id)
		  (error 'mixin
			 "expected to-interface, got: ~e; others ~e"
			 to-id
			 all-to)))
	      all-to))


  (define (check-interface-includes xs from-ids)
    (for-each
     (lambda (x)
       (unless (ormap (lambda (i) (method-in-interface? x i)) from-ids)
	 (error 'mixin
		"method `~a' was referenced in definition, but is not in any of the from-interfaces: ~e"
		x from-ids)))
     xs))
  
  (define-syntax (mixin stx)
    (syntax-case stx ()
      [(_ (from ...) (to ...) clauses ...)
       (let ([extract-renamed-names
              (λ (x)
                (map (λ (x) (syntax-case x ()
                              [(internal-name external-name) (syntax external-name)]
                              [else x]))
                     (syntax->list x)))])
         (define (get-super-names stx)
           (syntax-case stx (inherit rename 
                                     override overment override-final
                                     define/override define/overment define/override-final
                                     augment augride augment-final
                                     define/augment define/augride define/augment-final)
             [(inherit names ...) (extract-renamed-names (syntax (names ...)))]
             [(rename [x names] ...) (syntax->list (syntax (names ...)))]
             [(override names ...) (extract-renamed-names (syntax (names ...)))]
             [(overment names ...) (extract-renamed-names (syntax (names ...)))]
             [(override-final names ...) (extract-renamed-names (syntax (names ...)))]
             [(augment names ...) (extract-renamed-names (syntax (names ...)))]
             [(augride names ...) (extract-renamed-names (syntax (names ...)))]
             [(augment-final names ...) (extract-renamed-names (syntax (names ...)))]
             
             [(define/augment (name . names) . rest) (extract-renamed-names (syntax (name)))]
             [(define/augment name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
             [(define/augride (name . names) . rest) (extract-renamed-names (syntax (name)))]
             [(define/augride name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
             [(define/augment-final (name . names) . rest) (extract-renamed-names (syntax (name)))]
             [(define/augment-final name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
             [(define/override (name . names) . rest) (extract-renamed-names (syntax (name)))]
             [(define/override name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
             [(define/overment (name . names) . rest) (extract-renamed-names (syntax (name)))]
             [(define/overment name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
             [(define/override-final (name . names) . rest) (extract-renamed-names (syntax (name)))]
             [(define/override-final name . rest) (identifier? (syntax name)) (extract-renamed-names (syntax (name)))]
             [else null]))
         (with-syntax ([(from-ids ...) (generate-temporaries (syntax (from ...)))]
                       [(to-ids ...) (generate-temporaries (syntax (to ...)))]
                       [(super-vars ...)
                        (apply
                         append
                         (map get-super-names
                              (syntax->list (syntax (clauses ...)))))]
                       [mixin-name (or (with-syntax ([tmp (syntax-local-name)])
                                         (syntax (quote tmp)))
                                       (syntax (quote mixin)))])
           
           ;; Build the class expression first, to give it a good src location:
           (with-syntax ([class-expr
                          (with-syntax ([orig-stx stx])
                            (syntax/loc stx
                              (class/derived orig-stx [#f super% (to-ids ...) #f]
                                clauses ...)))])
             
             ;; Now build mixin proc, again to give it a good src location:
             (with-syntax ([mixin-expr
                            (syntax/loc stx
                              (λ (super%)
			        (check-mixin-super mixin-name super% (list from-ids ...))
                                class-expr))])
               
               ;; Finally, build the complete mixin expression:
               (syntax/loc stx
                 (let ([from-ids from] ...)
                   (let ([to-ids to] ...)
		     (check-mixin-from-interfaces (list from-ids ...))
		     (check-mixin-to-interfaces (list to-ids ...))
                     (check-interface-includes (list (quote super-vars) ...)
					       (list from-ids ...))
                     mixin-expr)))))))]))

  (define externalizable<%>
    (_interface () externalize internalize))

  ;; Providing traced versions:
  (provide class-traced
           class*-traced
           class/derived-traced
	   (rename define-serializable-class define-serializable-class-traced)
           (rename define-serializable-class* define-serializable-class*-traced)
           (rename mixin mixin-traced)
           new-traced
           make-object-traced
           instantiate-traced
	   send-traced
           send/apply-traced
           send*-traced
           class-field-accessor-traced
           class-field-mutator-traced
           with-method-traced
           get-field-traced
           field-bound?-traced
           field-names-traced
	   (rename generic/form generic-traced)
           (rename make-generic/proc make-generic-traced)
           send-generic-traced
	   is-a?-traced
           object-interface-traced
           object-info-traced
           object->vector-traced
	   object-method-arity-includes?-traced
	   )

  ;; Providing normal functionality:
  (provide (protect make-wrapper-class
		    wrapper-object-wrapped
		    extract-vtable
		    extract-method-ht)
           
           (rename _class class) class* class/derived
           define-serializable-class define-serializable-class*
           class?
           mixin
	   (rename _interface interface) interface?
	   object% object? object=? externalizable<%>
           new make-object instantiate
           get-field field-bound? field-names
	   send send/apply send* class-field-accessor class-field-mutator with-method
	   private* public*  pubment*
	   override* overment*
	   augride* augment*
	   public-final* override-final* augment-final*
	   define/private define/public define/pubment 
	   define/override define/overment
	   define/augride define/augment
	   define/public-final define/override-final define/augment-final
	   define-local-member-name define-member-name 
           member-name-key generate-member-key member-name-key? member-name-key=? member-name-key-hash-code
	   (rename generic/form generic) (rename make-generic/proc make-generic) send-generic
	   is-a? subclass? implementation? interface-extension?
	   object-interface object-info object->vector
           object-method-arity-includes?
	   method-in-interface? interface->method-names class->interface class-info
	   (struct exn:fail:object ())
	   make-primitive-class))

