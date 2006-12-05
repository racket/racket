;; lexical analysis phase of the compiler
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

;; For closures, variable sets, such as the free variables,
;;  are computed.
;; Constants are transformed into varrefs to top-level
;;  constant definitions. A list of these is kept, and
;;  code is generated to create the constants and prefixed
;;  onto the output code. See also const.ss.
;; Global and primitive varrefs are collected into a list,
;;  so they can all be looked up once.
;; Letrec expressions that don't just bind procedures are
;;  changed to let+set!. Letrecs that remain after this
;;  phase are well-behaved procedure binders.
;; Some simple arity checks are performed, and return-arity
;;  information is kept for closures. Ultimately, the return-
;;  arity information is used to compile away some return
;;  value count checks.
;; Constants are propagated.
;; Constant let-bound variables are eliminated (unless they
;;   are improperly used in the let body).
;; Procedure applications are inlined.
;; Syntax constants created, so quote-syntax turns into a
;;   varref

;;; Annotatitons: ----------------------------------------------
;;    binding - `binding' structure: UPDATED occasionally:
;;         rec? field for letrec bindings
;;         known-but-used? field for ill-used bindings
;;    quote - zodiac:varref (global var containing a constructed 
;;         constant) or an immediate constant
;;    app - `app' structure: UPDATED tail? flag
;;    lambda - `procedure-code' structure
;;    with-continuation-mark - might set annotation to #f, which
;;         indicates that begin0-like handling is not needed
;;    quote-syntax - zodiac:varref (global var containing a constructed 
;;         constant)
;;; ------------------------------------------------------------

(module analyze mzscheme
  (require (lib "unit.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide analyze@)
  (define-unit analyze@
      (import (prefix compiler:option: compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (prefix zodiac: zodiac^)
	      compiler:zlayer^
	      compiler:prephase^
	      compiler:anorm^
	      compiler:known^
	      compiler:const^
	      compiler:rep^
	      compiler:vm2c^
	      compiler:driver^)
      (export compiler:analyze^)
      
      (define-struct mod-glob (cname   ;; a made-up name that encodes module + var
			       modname
			       varname 
			       position
			       exp-time?
			       exp-def?
			       in-module?))

      (define-struct modref-info (globals
				  et-globals
				  modidx-const)) ; #f => local to module body

      (define compiler:global-symbols (make-hash-table))
      (define compiler:add-global-varref!
	(case-lambda 
	 [(varref) (compiler:add-global-varref!
		    (zodiac:top-level-varref-module varref)
		    (zodiac:varref-var varref)
		    varref
		    (zodiac:top-level-varref-exptime? varref)
		    (zodiac:top-level-varref-expdef? varref)
		    (zodiac:top-level-varref-position varref))]
	 [(modname varname ast et? ed? position)
	  (let* ([et? (and et?
			   ;; Just use run-time for #%kernel, since it's the same, and
			   ;;  the compiler generates references to #%kernel names
			   (not (eq? '#%kernel modname)))]
		 [info (hash-table-get compiler:global-symbols modname
				       (lambda ()
					 (let ([p (make-modref-info 
						   #f #f
						   (if (module-path-index? modname)
						       (let-values ([(name base) (module-path-index-split modname)])
							 (if name
							     (compiler:construct-const-code!
							      (zodiac:make-zread
							       (datum->syntax-object
								#f
								modname
								(zodiac:zodiac-stx ast)))
							      #t)
							     #f))
						       modname))])
					   (hash-table-put! compiler:global-symbols modname p)
					   p)))]
		 [t (or ((if et? modref-info-et-globals modref-info-globals) info)
			(let ([t (make-hash-table)])
			  ((if et? set-modref-info-et-globals! set-modref-info-globals!) info t)
			  t))])
	    (hash-table-get t varname
			    (lambda ()
			      ;; vm->c function also generates a symbol constant:
			      (let ([n (make-mod-glob (vm->c:generate-modglob-name modname varname)
						      modname varname position et? ed?
						      (and ast (varref:has-attribute? 
								ast
								varref:in-module)))])
				(unless ast
				  (compiler:internal-error
				   #f
				   "unexpected global name generation for ~a (~a;~a)" 
				   varname modname et?))
				(hash-table-put! t varname n)
				n))))]))

      (define (ensure-top-level varref)
	(if (zodiac:top-level-varref-module varref)
	    (begin
	      (compiler:warning
	       varref
	       (format "definition of name `~a' mapped by --prim or `require'; compiled uses of the name will ignore this definition"
		       (zodiac:varref-var varref)))
	      (let ([new (zodiac:make-top-level-varref
			  (datum->syntax-object
			   #f ; no context
			   (syntax-e (zodiac:zodiac-stx varref))
			   (zodiac:zodiac-stx varref))
			  (make-empty-box)
			  (zodiac:varref-var varref)
			  #f
			  (box #f) ; slot - fresh because the variable can't be referenced from anywhere
			  #f #f #f)])
		(set-annotation! new (varref:empty-attributes))
		new))
	    varref))

      (define (compiler:get-module-path-constant modname)
	(modref-info-modidx-const (hash-table-get compiler:global-symbols modname void)))

      (define (compiler:get-global-symbols) compiler:global-symbols)

      (define compiler:primitive-refs empty-set)
      (define compiler:add-primitive-varref!
	(lambda (varref)
	  (set! compiler:primitive-refs
		(set-union-singleton compiler:primitive-refs
				     (zodiac:varref-var varref)))))
      (define (compiler:get-primitive-refs) compiler:primitive-refs)

      (define compiler:max-arity-allowed 11739) ; meant to be a sane limit
      
      (define compiler:define-list null)
      (define compiler:per-load-define-list null)
      (define compiler:local-define-list null)
      (define compiler:local-per-load-define-list null)

      (define (compiler:get-define-list) compiler:define-list)
      (define (compiler:get-per-load-define-list) compiler:per-load-define-list)
      
      (define (compiler:init-define-lists!)
	(set! compiler:define-list null)
	(set! compiler:per-load-define-list null)
	(set! compiler:global-symbols (make-hash-table))
	(set! compiler:primitive-refs empty-set))
      
      (define (compiler:add-local-define-list! def)
	(set! compiler:local-define-list 
	      (cons def compiler:local-define-list)))
      (define (compiler:add-local-per-load-define-list! def)
	(set! compiler:local-per-load-define-list 
	      (cons def compiler:local-per-load-define-list)))

      (define (prepare-local-lists)
	(set! compiler:local-define-list null)
	(set! compiler:local-per-load-define-list null))

      (define (move-over-local-lists)
	(set! compiler:define-list
	      (append! compiler:define-list 
		       (reverse! compiler:local-define-list)))
	(set! compiler:per-load-define-list
	      (append! compiler:per-load-define-list 
		       (reverse! compiler:local-per-load-define-list)))
	
	(set! compiler:local-define-list null)
	(set! compiler:local-per-load-define-list null))
      
      ;; Temporary structure used in building up case-lambda info
      (define-struct case-info 
	(body case-code global-vars used-vars captured-vars max-arity return-multi))

      (define (list->zodiac:quote l ast)
	(zodiac:make-quote-form
	 (zodiac:zodiac-stx ast)
	 (make-empty-box)
	 (zodiac:structurize-syntax l ast)))

      ;; Turns a virtual set-values! into an expression using normal
      ;; set!s. The result must be a-normalized, have the right
      ;; binding annotations, and the right known-value links.
      (define letrec-multiple-set!->single-set!
	(lambda (orig-zbindings vars val ast)
	  (cond
	   [(null? vars)
	    ;; zero value set!  -- weirdos
	    (zodiac:make-let-values-form
	     (zodiac:zodiac-stx ast)
	     (make-empty-box)
	     (list null)
	     (list val)
	     (zodiac:make-special-constant 'void))]
	   
	   [else
	    ;; single or multiple value set! 
	    (let* ([names (map (lambda (_) (gensym)) vars)]
		   [zbindings (map (lambda (name orig-zbinding)
				     (let ([zb (zodiac:make-lexical-binding
						(zodiac:zodiac-stx ast)
						(make-empty-box)
						name
						name)])
				       (let ([old-binding (get-annotation orig-zbinding)]
					     [new-binding (make-unknown-letbound-binding #f)])
					 (set-annotation! zb new-binding)
					 ;; Copy known info from the old binding to the new one.
					 (when (binding-known? old-binding)
					   (set-binding-known?! new-binding #t)
					   (set-binding-val! new-binding (binding-val old-binding))))
				       zb))
				   names orig-zbindings)])
	      ;; The original bindings will be set!ed, so they must now be marked as
	      ;;  a special kind of "mutable" for boxing.
	      (map (lambda (zb) (set-binding-letrec-set?! (get-annotation zb) #t)) orig-zbindings)
	      ;; If it's one binding, make sure known? is set:
	      (when (= 1 (length zbindings))
		(let ([binding (get-annotation (car zbindings))])
		  (unless (binding-known? binding)
		    (set-binding-known?! binding #t)
		    (set-binding-val! binding val))))
	      ;; Make the new expession
	      (zodiac:make-let-values-form
	       (zodiac:zodiac-stx ast)
	       (make-empty-box)
	       (list zbindings)
	       (list val)
	       (let ([set!s (let loop ([zbindings zbindings] [vars vars])
			      (if (null? zbindings)
				  null
				  (cons
				   (zodiac:make-set!-form
				    (zodiac:zodiac-stx ast)
				    (make-empty-box)
				    (car vars)
				    (zodiac:binding->lexical-varref
				     (car zbindings)))
				   (loop (cdr zbindings) (cdr vars)))))])
		 (if (= 1 (length set!s))
		     (car set!s)
		     (zodiac:make-begin-form
		      (zodiac:zodiac-stx ast)
		      (zodiac:parsed-back ast)
		      set!s)))))])))

      ;; turns a 'bad' letrec into let+set!, also returning a procedure
      ;; to set! the body to the correct form, to avoid re-analyzing it
      ;; the 'body' is set to (void)
      (define letrec->let+set!
	(lambda (ast)
	  (let* ([body
		  (zodiac:make-begin-form
		   (zodiac:zodiac-stx ast)
		   (make-empty-box)
		   (let linearize-set! ([varses (zodiac:letrec-values-form-vars ast)]
					[vals (zodiac:letrec-values-form-vals ast)])
		     (if (null? varses)
			 (list (zodiac:letrec-values-form-body ast))
			 (let* ([vars (car varses)] 
				[val (car vals)])
			   (cons
			    ;; must turn set!-values into set! form
			    (letrec-multiple-set!->single-set!
			     vars
			     (map zodiac:binding->lexical-varref vars)
			     val
			     ast)
			    (linearize-set! (cdr varses) (cdr vals)))))))]
		 [let-form
		  ;; In this phase, we must only construct let-vales forms with one clause
		  (let loop ([varses (zodiac:letrec-values-form-vars ast)])
		    (if (null? varses)
			body
			(let ([vars (car varses)])
			  (zodiac:make-let-values-form 
			   (zodiac:zodiac-stx ast)
			   (make-empty-box) ; (zodiac:parsed-back ast)
			   (list vars)
			   (list
			    (if (= 1 (length vars))
				(zodiac:make-special-constant 'undefined)
				(compiler:make-const-constructor 
				 ast
				 'values
				 (map (lambda (_) (zodiac:make-special-constant 
						   'undefined))
				      vars))))
			   (loop (cdr varses))))))])
	    let-form)))

      ;; Tells us which constants we can replace directly with their text
      (define (can-propagate-constant? ast)
	(or (zodiac:quote-form? ast)
	    ;; (zodiac:lexical-varref? ast) to do this, we must put renamed vars in
	    ;; environments as their old name...
	    (and (zodiac:top-level-varref? ast)
		 (or (varref:has-attribute? ast varref:primitive)
		     (varref:has-attribute? ast varref:static)))))

      ;; Which expressions can we drop entirely (e.g., RHS of a let when the value is known)?
      (define (can-drop-expression? ast)
	(or (zodiac:quote-form? ast)
	    (zodiac:bound-varref? ast)
	    (and (zodiac:top-level-varref? ast)
		 (varref:has-attribute? ast varref:static))))

      (define (or-multi a-multi b-multi)
	(case a-multi
	  [(#f) b-multi]
	  [(possible) (or b-multi 'possible)]
	  [(#t) #t]))

      ;;----------------------------------------------------------------------
      ;; INLINING

      ;; Only inline fairly simple things; also check for variables needed
      ;;  that are not in the destination scope
      (define (expression-inline-cost body env init-size)
	(let loop ([body body][env env][size init-size][k (lambda (x) x)])
	  (if (>= size (compiler:option:max-inline-size))
	      size
	      (cond
	       [(zodiac:quote-form? body) (k size)]
	       [(zodiac:bound-varref? body)
		(if (memq (zodiac:bound-varref-binding body) env)
		    (k size)
		    ;; Out of scope - no inling
		    (* 2 (compiler:option:max-inline-size)))]
	       [(zodiac:varref? body) (k size)]
	       [(zodiac:app? body) (loop (zodiac:app-fun body) 
					 env
					 (+ size  (length (zodiac:app-args body)) 1)
					 (lambda (size)
					   (let kloop ([l (zodiac:app-args body)][size size])
					     (if (null? l)
						 (k size)
						 (loop (car l) env size
						       (lambda (size)
							 (kloop (cdr l) size)))))))]
	       [(zodiac:if-form? body) (loop (zodiac:if-form-test body) env (+ size 3)
					     (lambda (size)
					       (loop (zodiac:if-form-else body) env size
						     (lambda (size)
						       (loop (zodiac:if-form-then body) env size k)))))]
	       [(zodiac:set!-form? body) (loop (zodiac:set!-form-val body) env (+ size 3) k)]
	       [(zodiac:let-values-form? body) (loop (car (zodiac:let-values-form-vals body)) 
						     env
						     (+ size 3)
						     (lambda (size)
						       (loop (zodiac:let-values-form-body body)
							     (append (car (zodiac:let-values-form-vars body)) env)
							     size k)))]
	       [(zodiac:begin-form? body) (let bloop ([size size][exprs (zodiac:begin-form-bodies body)])
					    (if (null? exprs)
						(k size)
						(loop (car exprs)
						      env
						      (+ size 1)
						      (lambda (size)
							(bloop size (cdr exprs))))))]
	       [(zodiac:global-lookup? body) (k size)]
	       [(zodiac:safe-vector-ref? body) (k size)]
	       [(zodiac:global-assign? body) (loop (zodiac:global-assign-expr body) env (+ size 1) k)]
	       [else (* 2 (compiler:option:max-inline-size))]))))

      ;; We copy inlined bodies to generate unique structure values
      ;;  for every distinct program point. But we don't copy
      ;;  quote forms, so the same run-time value will be used for
      ;;  the quote form in all its instantiations. We also don't
      ;;  know whether it's been analyzed in this phase, yet.
      (define (copy-inlined-binding ast)
	(let ([b (zodiac:make-lexical-binding
		  (zodiac:zodiac-stx ast)
		  (make-empty-box)
		  (gensym (string-append (symbol->string (zodiac:binding-var ast)) "i"))
		  (zodiac:binding-orig-name ast))]
	      [a (get-annotation ast)])
	  (set-annotation! b (copy-binding a))
	  b))
      (define (copy-inlined-body ast binding-map)
	(cond
	 [(or (and (zodiac:lexical-varref? ast) 
		   zodiac:make-lexical-varref))
	  =>
	  (lambda (f)
	    (let* ([binding (let* ([binding (zodiac:bound-varref-binding ast)]
				   [remapped (assq binding binding-map)])
			      (if remapped
				  (cdr remapped)
				  binding))]
		   [new-ast (f (zodiac:zodiac-stx ast)
			       (make-empty-box)
			       (zodiac:binding-var binding)
			       binding)])
	      ;; Copy attribute set:
	      (set-annotation! new-ast (get-annotation ast))
	      new-ast))]
	 [(zodiac:top-level-varref? ast)
	  (let ([new-ast (zodiac:make-top-level-varref
			  (zodiac:zodiac-stx ast)
			  (make-empty-box)
			  (zodiac:varref-var ast)
			  (zodiac:top-level-varref-module ast)
			  (zodiac:top-level-varref-slot ast)
			  (zodiac:top-level-varref-exptime? ast)
			  (zodiac:top-level-varref-expdef? ast)
			  (zodiac:top-level-varref-position ast))])
	    ;; Copy attribute set:
	    (set-annotation! new-ast (get-annotation ast))
	    new-ast)]
	 [(zodiac:quote-form? ast) ast]
	 [(zodiac:app? ast)
	  (let ([new-ast (zodiac:make-app
			  (zodiac:zodiac-stx ast)
			  (make-empty-box)
			  (copy-inlined-body (zodiac:app-fun ast) binding-map)
			  (map (lambda (x) (copy-inlined-body x binding-map)) (zodiac:app-args ast)))]
		[appinfo (get-annotation ast)])
	    (set-annotation! new-ast
			     (make-app #f
				       (app-prim? appinfo)
				       (app-prim-name appinfo)))
	    new-ast)]
	 [(zodiac:if-form? ast)
	  (zodiac:make-if-form
	   (zodiac:zodiac-stx ast)
	   (make-empty-box)
	   (copy-inlined-body (zodiac:if-form-test ast) binding-map)
	   (copy-inlined-body (zodiac:if-form-then ast) binding-map)
	   (copy-inlined-body (zodiac:if-form-else ast) binding-map))]
	 [(zodiac:set!-form? ast)
	  (zodiac:make-set!-form
	   (zodiac:zodiac-stx ast)
	   (make-empty-box)
	   (copy-inlined-body (zodiac:set!-form-var ast) binding-map)
	   (copy-inlined-body (zodiac:set!-form-val ast) binding-map))]
	 [(zodiac:let-values-form? ast)
	  (let* ([vars (car (zodiac:let-values-form-vars ast))]
		 [val (car (zodiac:let-values-form-vals ast))]
		 [new-val (copy-inlined-body val binding-map)]
		 [new-vars (map copy-inlined-binding vars)]
		 [new-binding-map (append (map cons vars new-vars) binding-map)]
		 [new-body (copy-inlined-body (zodiac:let-values-form-body ast) new-binding-map)])
	    
	    ;; Update known-value information; if it was known to be = to a varref or binding,
	    ;; we may have replaced it with a new varref or binding.
	    (when (= 1 (length new-vars))
	      (let ([binding (get-annotation (car new-vars))])
		(when (binding-known? binding)
		  (let ([known (binding-val binding)])
		    (when (or (zodiac:bound-varref? known)
			      (zodiac:binding? known))
		      (let* ([known-zbinding (if (zodiac:binding? known)
						 known
						 (zodiac:bound-varref-binding known))]
			     [found (assq known-zbinding binding-map)])
			(when found
			  ;; Yep - that varref/binding was replaced
			  (set-binding-val! binding (cdr found)))))))))
	    
	    (zodiac:make-let-values-form
	     (zodiac:zodiac-stx ast)
	     (make-empty-box)
	     (list new-vars)
	     (list new-val)
	     new-body))]
	 [(zodiac:begin-form? ast)
	  (zodiac:make-begin-form
	   (zodiac:zodiac-stx ast)
	   (make-empty-box)
	   (map (lambda (x) (copy-inlined-body x binding-map))
		(zodiac:begin-form-bodies ast)))]
	 [(zodiac:global-lookup? ast)
	  (zodiac:make-global-lookup
	   (zodiac:zodiac-stx ast)
	   (make-empty-box)
	   (copy-inlined-body (zodiac:global-lookup-vec ast) binding-map)
	   (zodiac:global-lookup-pos ast))]
	 [(zodiac:safe-vector-ref? ast)
	  (zodiac:make-safe-vector-ref
	   (zodiac:zodiac-stx ast)
	   (make-empty-box)
	   (copy-inlined-body (zodiac:safe-vector-ref-vec ast) binding-map)
	   (zodiac:safe-vector-ref-pos ast))]
	 [(zodiac:global-assign? ast)
	  (zodiac:make-global-assign
	   (zodiac:zodiac-stx ast)
	   (make-empty-box)
	   (copy-inlined-body (zodiac:global-assign-vec ast) binding-map)
	   (zodiac:global-assign-pos ast)
	   (copy-inlined-body (zodiac:global-assign-expr ast) binding-map))]
	 [else (compiler:internal-error
		ast
		(format "copy-inlined-body: can't copy ~a"
			ast))]))

      (define (check-for-inlining ast env inlined tail? inline dont-inline)
	(if (>= inlined (compiler:option:max-inline-size))
	    (dont-inline "depth")
	    (let ([c (zodiac:app-fun ast)])
	      (if (not (zodiac:bound-varref? c))
		  (dont-inline `(format "not a varref: ~a" ,c))
		  (let* ([val (extract-varref-known-val c)])
		    (if (not (zodiac:case-lambda-form? val))
			(dont-inline `(format "not a lambda: ~a" ,val))
			;; We're going to inline the body if the arg count matches
			(let loop ([argses (zodiac:case-lambda-form-args val)]
				   [bodies (zodiac:case-lambda-form-bodies val)])
			  (cond
			   [(null? argses) (dont-inline "no arg match")]
			   [(and (zodiac:list-arglist? (car argses))
				 (= (length (zodiac:app-args ast))
				    (length (zodiac:arglist-vars (car argses)))))
			    ;; Inline if lexical scope of destination includes the lexical
			    ;;  scope of the source, and simple enough
			    (let* ([body (car bodies)]
				   [orig-vars (zodiac:arglist-vars (car argses))]
				   [new-size (expression-inline-cost 
					      body (append orig-vars env) 
					      (if tail?
						  (max 5 inlined)
						  inlined))])
			      (if (< new-size (compiler:option:max-inline-size))
				  (let* ([vars (map copy-inlined-binding orig-vars)]
					 [vals (zodiac:app-args ast)]
					 [new-body (copy-inlined-body body (map cons orig-vars vars))]
					 [v (let loop ([vars vars][vals vals])
					      (if (null? vars)
						  new-body
						  (zodiac:make-let-values-form
						   (zodiac:zodiac-stx ast)
						   (make-empty-box)
						   (list (list (car vars)))
						   (list (car vals))
						   (loop (cdr vars) (cdr vals)))))])
				    ;; Unless mutable, the new bindings have known
				    ;;  value expressions
				    (for-each
				     (lambda (var val)
				       (let ([binding (get-annotation var)])
					 (unless (binding-mutable? binding)
					   (set-binding-known?! binding #t)
					   (set-binding-val! binding val))))
				     vars (extract-ast-known-value vals))
				    (inline v new-size))
				  (dont-inline "too complex")))]
			   [else (loop (cdr argses) (cdr bodies))]))))))))
      

      ;;----------------------------------------------------------------------
      ;; analyze-expression takes 4 arguments
      ;;  1) an AST to transform
      ;;  2) a set of bound variables (the lexical environment)
      ;;  3) tail?
      ;;
      ;; it returns 6 values
      ;;  1) a destructively altered AST
      ;;  2) a set of variables occurring free in that expression [free-vars]
      ;;  3) a set of variables which are bound by lets in that expression [local-vars]
      ;;  4) global variables and mutable `constants' used in the expression [global-vars]
      ;;  5) local variables (including given bound set) that are used directly or captured by nested closures [used-vars]
      ;;  6) free and local variables that are captured by nested closures [captured-vars]
      ;;  7) a list of `code' structures
      ;;  8) maximum arity to a call made in this expression
      ;;  9) returns multiple values?: #t, 'possible, or #f
      ;;
      (define analyze-expression!
	(lambda (ast just-bound-vars env tail?)
	  (let ([local-vars just-bound-vars]
		[locals-used empty-set]
		[captured-vars empty-set]
		[free-vars empty-set]
		[global-vars empty-set]
		[codes null]
		[max-arity 1])
	    (letrec
		([add-local-var! (lambda (var)
				   (set! local-vars
					 (set-union-singleton local-vars var)))]
		 [remove-local-var! (lambda (var)
				      (set! local-vars
					    (set-minus local-vars (make-singleton-set var))))]
		 [add-free-var! (lambda (var)
				  (set! free-vars
					(set-union-singleton free-vars var)))]
		 [add-used-var! (lambda (var)
				  (set! locals-used
					(set-union-singleton locals-used var)))]
		 [add-global-var! (lambda (var)
				    (set! global-vars
					  (set-union-singleton global-vars var)))]
		 [add-child-code! (lambda (c)
				    (set! codes (cons c codes)))]
		 [register-arity! (lambda (n) (set! max-arity (max n max-arity)))]  
		 [register-code-vars!
		  (lambda (code can-lift?)
		    ;; all variables which are free in nested closures are
		    ;; free in this closure (since we need them in the
		    ;; environment) except those that are already in the
		    ;; environment: just-bound-vars (typically the arguments)
		    ;; and local-vars. New captured variables are also
		    ;; added: free variables and captured variables in
		    ;; nested closures. (Captured variables are always
		    ;; a subset of free + used.)
		    (set! free-vars
			  (set-union (set-minus (code-free-vars code)
						local-vars)
				     free-vars))
		    (set! locals-used
			  (set-union (set-intersect local-vars
						    (code-free-vars code))
				     locals-used))
		    (set! captured-vars
			  (set-union (set-minus (code-free-vars code)
						local-vars)
				     captured-vars))
		    (set! global-vars
			  (set-union (code-global-vars code) global-vars)))]
		 [analyze-code-body!
		  (lambda (ast locals env tail? code)
		    (add-child-code! code)
		    (let-values ([(body free-vars
					local-vars 
					global-vars 
					used-vars
					captured-vars
					children
					L-max-arity
					multi)
				  (analyze-expression! ast
						       locals
						       (append (set->list locals) env)
						       tail?)])
		      
		      (set-code-free-vars! code (set-union free-vars
							   (code-free-vars code)))
		      (set-code-local-vars! code (set-union local-vars
							    (code-local-vars code)))
		      (set-code-global-vars! code (set-union global-vars
							     (code-global-vars code)))
		      (set-code-used-vars! code (set-union used-vars
							   (code-used-vars code)))
		      (set-code-captured-vars! code (set-union captured-vars
							       (code-captured-vars code)))
		      (set-code-children! code children)
		      (for-each (lambda (c) (set-code-parent! c code)) children)
		      (set-closure-code-max-arity! code (max L-max-arity
							     (closure-code-max-arity code)))
		      (set-closure-code-return-multi! code multi)
		      
		      body))]
		 [analyze-varref!
		  (lambda (ast env tail? need-varref?)
		    (cond
		     
		     ;;-----------------------------------------------------------------
		     ;; VARIABLE REFERENCES (A-VALUES)
		     ;;
		     ;;    We need to catalogue which variables are used in this 
		     ;;    expression.  if a lexical varref is not in the 
		     ;;    environment, it is free
		     ;;
		     ;;    if the bound structure indicates this variable is known at
		     ;;    compile time, replace the varref with the const-ref
		     ;;    since we can propagate varrefs, we need to make sure we
		     ;;    capture the right name in closures.
		     ;;
		     [(zodiac:bound-varref? ast)
		      
		      ;; check to see if it's known.  If so, just return the known value.
		      
		      (let* ([zbinding (zodiac:bound-varref-binding ast)]
			     [binding (compiler:bound-varref->binding ast)]
			     [known-value (extract-varref-known-val ast)])
			
			;; Extra checking for debugging:
			;; (unless (memq zbinding env) (compiler:internal-error ast "unbound variable"))
			
			(cond 
			 [(and (compiler:option:propagate-constants)
			       known-value 
			       (can-propagate-constant? known-value)
			       (not need-varref?))
			  
			  ;; Propogate a mzc-determined constant!
			  ;; This could be a quote-form that was installed
			  ;; as a known value before it was
			  ;; analyzed. If so, extract the constructed
			  ;; constant from the backbox.
			  ;; In any case, check for adding PLS to the closure.
			  (let ([c (if (zodiac:quote-form? known-value)
				       
				       (let ([a (get-annotation known-value)])
					 (if (zodiac:varref? a)
					     a
					     (analyze-quote! known-value #f)))

				       known-value)])
			    
			    (when (and (zodiac:varref? c)
				       (varref:has-attribute? c varref:per-load-static))
			      (add-global-var! const:the-per-load-statics-table))
			    
			    c)]
			 
			 [else   
			  ;; otherwise we don't know the value -- therefore just
			  ;; do the normal free-variable analysis
			  
			  (if (not (set-memq? (zodiac:bound-varref-binding ast)
					      local-vars))
			      
			      (begin 
				(add-free-var! (zodiac:bound-varref-binding ast))
				(varref:add-attribute! ast varref:env)
				
				;; If this variable has an anchor, include it in the list of free vars
				(let ([a (binding-anchor (get-annotation (zodiac:bound-varref-binding ast)))])
				  (when a
				    (add-free-var! a)))
				
				ast)
			      
			      (begin
				(add-used-var! (zodiac:bound-varref-binding ast))
				
				;; If this variable has an anchor, include it in the list of used vars
				(let ([a (binding-anchor (get-annotation (zodiac:bound-varref-binding ast)))])
				  (when a
				    (add-used-var! a)))
				
				ast))]))]
		     
		     [(zodiac:top-level-varref? ast)
		      
		      ;; A varref may need to generate module-index values.
		      (prepare-local-lists)

		      (cond
		       [(varref:has-attribute? ast varref:primitive)
			(compiler:add-primitive-varref! ast)]
		       [(varref:has-attribute? ast varref:per-load-static)
			(add-global-var! const:the-per-load-statics-table)]
		       [(varref:has-attribute? ast varref:static)
			(void)]
		       [else
			(add-global-var! (compiler:add-global-varref! ast))])
		      (compiler:add-global-varref! ast)

		      ;; Was a module-index value generated?
		      (move-over-local-lists)

		      ast]
		     
		     [else (compiler:internal-error 
			    ast
			    "analyze: expected a variable; got ~a" ast)]))]
		 
		 ;;-----------------------------------------------------------------
		 ;; CONSTANTS (A-VALUES)
		 ;;   literal constants -- send them off to the constant
		 ;;   constructors!!  This produces code in b-normal form
		 ;;   and adds defines to the local-define-list. This list
		 ;;   must be reversed since the dependencies are backwards
		 ;;   ahh, the excitement of multiple values...
		 [analyze-quote!
		  (lambda (ast known-immutable?)
		    (prepare-local-lists)
		    (let ([ret (compiler:construct-const-code! 
				(zodiac:quote-form-expr ast)
				(or known-immutable?
				    (eq? 'immutable (get-annotation ast))))])
		      ;; Put a pointer to the constructed constant in the quote-form's backbox
		      (set-annotation! ast ret)
		      
		      (move-over-local-lists)

		      ;; If this `constant' is mutable, register the per-load
		      ;; statics pointer as a `global'
		      (when (and (zodiac:top-level-varref? ret)
				 (varref:has-attribute? ret varref:per-load-static))
			(add-global-var! const:the-per-load-statics-table))
		      
		      ret))]
		 
		 [analyze!-ast
		  ;; Like analyze, but drop the multi-return info in the result
		  (lambda (ast env inlined)
		    (let-values ([(ast multi) (analyze! ast env inlined #f #f)])
		      ast))]
		 
		 [analyze!-sv
		  ;; Like analyze, but make sure the expression is not definitely
		  ;;  multi-valued
		  (lambda (ast env inlined)
		    (let-values ([(ast multi) (analyze! ast env inlined #f #f)])
		      (when (eq? multi #t)
			((if (compiler:option:stupid) compiler:warning compiler:error)
			 ast
			 "returning zero or multiple values to a context expecting 1 value"))
		      ast))]
		 
		 [analyze!
		  ;; Returns (values ast multi)
		  ;;    where multi = #f, #t, 'possible
		  (lambda (ast env inlined tail? wcm-tail?)
		    (when (compiler:option:debug)
		      (zodiac:print-start! (debug:get-port) ast)
		      (newline (debug:get-port)))
		    
		    (cond
		     
		     ;;-----------------------------------------------------------------
		     ;; CONSTANTS (A-VALUES)
		     [(zodiac:quote-form? ast)
		      (values (analyze-quote! ast #f) #f)]
		     
		     ;;-----------------------------------------------------------------
		     ;; VARIABLE REFERENCES (A-VALUES)
		     ;;
		     ;;    We need to catalogue which variables are used in this 
		     ;;    expression.  if a lexical varref is not in the 
		     ;;    environment, it is free
		     ;;
		     ;;    if the bound structure indicates this variable is known at
		     ;;    compile time, replace the varref with the const-ref
		     ;;    since we can propagate varrefs, we need to make sure we
		     ;;    capture the right name in closures.
		     ;;
		     [(zodiac:bound-varref? ast) 
		      (values (analyze-varref! ast env tail? #f) #f)]

		     [(zodiac:top-level-varref? ast)
		      (values (analyze-varref! ast env tail? #f) #f)]
		     
		     ;;--------------------------------------------------------------------
		     ;; LAMBDA EXPRESSIONS
		     ;;    with lambda, we need to make a recursive call.  We
		     ;;    extend the lexical environment with everything that's
		     ;;    been declared locally so far.  From the analyze-expression
		     ;;    we have information about what free variables they use	
		     ;;
		     [(zodiac:case-lambda-form? ast)
		      (let* ([code
			      (make-procedure-code empty-set empty-set empty-set empty-set empty-set
						   'unknown-proc-parent #f null
						   #f #f #f #f 0 #f (get-annotation ast) ; ann. = name
						   'unknown-case-infos #f 
						   'unknown-liftable
						   (and (syntax-property (zodiac:zodiac-stx ast) 'method-arity-error)
							;; Make sure that no case accepts 0 arguments:
							(andmap
							 (lambda (a) (not (null? (zodiac:arglist-vars a))))
							 (zodiac:case-lambda-form-args ast))))]
			     [case-infos
			      (map
			       (lambda (args body)
				 (let ([args (zodiac:arglist-vars args)])
				   
				   (let-values 
				       ([(lambda-body free-lambda-vars
						      local-lambda-vars 
						      global-lambda-vars 
						      used-lambda-vars
						      captured-lambda-vars
						      children-codes
						      L-max-arity
						      multi)
					 
					 (let ([just-bound (improper-list->set args)])
					   (analyze-expression! body
								just-bound
								(append (set->list just-bound) env)
								#t))])
				     
				     (let ([case-code (make-case-code
						       free-lambda-vars
						       local-lambda-vars
						       global-lambda-vars
						       used-lambda-vars
						       captured-lambda-vars
						       code #f children-codes
						       #f)])
				       (for-each (lambda (c) (set-code-case-parent! c case-code))
						 children-codes)
				       (make-case-info lambda-body case-code 
						       global-lambda-vars
						       used-lambda-vars 
						       captured-lambda-vars 
						       L-max-arity
						       multi)))))
			       (zodiac:case-lambda-form-args ast)
			       (zodiac:case-lambda-form-bodies ast))]
			     [case-codes (map case-info-case-code case-infos)]
			     [all-children (apply append (map code-children case-codes))])
			(set-procedure-code-case-codes! code case-codes)
			(set-code-children! code all-children)
			(for-each (lambda (c) (set-code-parent! c code)) all-children)
			(add-child-code! code)
			(let loop ([l case-infos])
			  (if (null? l)
			      (begin
				;; set the body
				(zodiac:set-case-lambda-form-bodies! ast (map case-info-body case-infos))

				;; now annotate this lambda form with the code
				(set-annotation! ast code)

				;; Propogate free, used, and captured vars:
				(register-code-vars! code #t)

				;; finally return it
				(values ast #f))
			      
			      (begin
				(set-code-free-vars! 
				 code
				 (set-union (code-free-vars
					     (case-info-case-code (car l)))
					    (code-free-vars code)))
				(set-code-local-vars! 
				 code
				 (set-union (code-local-vars
					     (case-info-case-code (car l)))
					    (code-local-vars code)))
				(set-code-global-vars!
				 code
				 (set-union (case-info-global-vars (car l))
					    (code-global-vars code)))
				(set-code-used-vars!
				 code
				 (set-union (case-info-used-vars (car l))
					    (code-used-vars code)))
				(set-code-captured-vars!
				 code
				 (set-union (case-info-captured-vars (car l))
					    (code-captured-vars code)))
				(set-closure-code-max-arity!
				 code
				 (max (closure-code-max-arity code)
				      (case-info-max-arity (car l))))
				(set-closure-code-return-multi! 
				 code
				 (or-multi (closure-code-return-multi code)
					   (case-info-return-multi (car l))))
				(loop (cdr l))))))]
		     
		     ;;--------------------------------------------------------------
		     ;; LET EXPRESSIONS
		     ;;    keep track of the bindings introduced so that each
		     ;;    expression can keep track of all the bindings it needs
		     ;;    this flattens environments
		     ;;    Several values may be bound at once.
		     ;;
		     ;;    in let, variables are assumed to be
		     ;;    immutable and known; we store this information
		     ;;    in the binding structure in the compiler:bound structure..
		     ;;
		     ;;    (let ([x (set! y A)]) M) ->
		     ;;      (begin (set! y A) (let ([x (void)]) M))
		     ;;
		     ;;    if the variable bound is constant, the let is discarded,
		     ;;    and the value is naturally propagated.
		     ;;
		     [(zodiac:let-values-form? ast)
		      (let*-values ([(val val-multi) 
				     (analyze! (car (zodiac:let-values-form-vals ast)) env inlined #f #f)]
				    [(vars) (car (zodiac:let-values-form-vars ast))]
				    [(convert-set!-val)
				     (lambda ()   
				       (set-car! (zodiac:let-values-form-vals ast)
						 (zodiac:make-special-constant 
						  'void))
				       (zodiac:make-begin-form 	   
					(zodiac:zodiac-stx ast)
					(make-empty-box)
					(list val ast)))])
			
			(if (= 1 (length (car (zodiac:let-values-form-vars ast))))
			    
					; this is a one-value binding let
			    (let* ([var (car vars)]
				   [binding (get-annotation var)])
			      
			      (when (eq? val-multi #t)
				((if (compiler:option:stupid) compiler:warning compiler:error)
				 ast
				 "returning zero or multiple values to a context expecting 1 value"))
			      
			      (add-local-var! var)
			      
			      (let-values ([(body body-multi)
					    (analyze! (zodiac:let-values-form-body ast) 
						      (cons var env)
						      inlined tail? wcm-tail?)]
					   [(known-val) (extract-varref-known-val var)])
				
				(if (and (compiler:option:propagate-constants)
					 (not (binding-mutable? binding))
					 known-val
					 (can-propagate-constant? known-val)
					 (can-drop-expression? val)
					 ;; can't eliminiate if a letrec->let variable
					 (not (binding-letrec-set? binding))
					 ;; can't eliminate if it is used by a bad application
					 (not (binding-known-but-used? binding)))
				    
				    ;; discard the let:
				    (begin
				      (remove-local-var! var)
				      (values body body-multi))

				    ;; otherwise, process normally
				    (begin
				      
				      (set-car! (car (zodiac:let-values-form-vars ast)) 
						var)
				      (zodiac:set-let-values-form-body! ast body)
				      
				      (if (zodiac:set!-form? val)
					  
					  ;; if we're binding the result of a set!-form, 
					  ;; turn it into
					  ;; a void.
					  (values (convert-set!-val) #f)
					  
					  ;; if it's any other expression, we're done.
					  (begin
					    (set-car! (zodiac:let-values-form-vals ast)
						      val)
					    (values ast body-multi)))))))
			    
			    ;; this is a multiple (or zero) value binding let
			    ;; the values are unknown to simple analysis so skip
			    ;; that stuff
			    (begin
			      (set-car! (zodiac:let-values-form-vars ast) vars)
					; these are all new bindings
			      (for-each add-local-var! vars)
					; analyze the body
			      (let-values ([(body body-multi) 
					    (analyze! (zodiac:let-values-form-body ast)
						      (append vars env)
						      inlined
						      tail? wcm-tail?)])
				(zodiac:set-let-values-form-body! ast body)
				
				(if (zodiac:set!-form? val)
				    (begin
				      ((if (compiler:option:stupid) compiler:warning compiler:error)
				       val
				       (format
					"returning 1 value (void) to a context expecting ~a values"
					(length vars)))
				      (when (compiler:option:stupid)
					(values (convert-set!-val) #f)))
					; if it's any other option, we're done
				    (begin
				      (set-car! (zodiac:let-values-form-vals ast) val)
				      (values ast body-multi)))))
			    
			    ))]

		     ;;-----------------------------------------------------------------
		     ;; LETREC EXPRESSIONS
		     ;;
		     ;; if the letrec form binds only lambda values and those bindings
		     ;; are not mutable, we keep this as a letrec, otherwise we 
		     ;; transform it to a let+set! combination as R4RS.
		     ;;
		     [(zodiac:letrec-values-form? ast)

		      (if (and 
			   ;; Well-behaved if everything's a closure
			   (andmap zodiac:case-lambda-form? 
				   (zodiac:letrec-values-form-vals ast))
			   ;; and all are one-variable bindings
			   (andmap (lambda (l) (= 1 (length l))) 
				   (zodiac:letrec-values-form-vars ast))
			   ;; and all are immutable
			   (andmap (lambda (l)
				     (not (binding-mutable? (get-annotation (car l)))))
				   (zodiac:letrec-values-form-vars ast)))
			  
			  ;;-----------------------------------------------------------
			  ;; WELL-BEHAVED LETREC (incomplete bindings never exposed)
			  ;;  mark appropriate variables as letrec bound
			  ;;
			  (let* ([vars (map car (zodiac:letrec-values-form-vars ast))])
			    (set! local-vars (set-union (list->set vars) local-vars))
			    (let ([new-env (append vars env)])
			      (let-values ([(vals) (map (lambda (val)
							  (analyze!-sv val new-env inlined))
							(zodiac:letrec-values-form-vals ast))]
					   [(body body-multi) (analyze! 
							       (zodiac:letrec-values-form-body ast) 
							       new-env
							       inlined
							       tail? wcm-tail?)]
					   [(vars) (map car (zodiac:letrec-values-form-vars ast))])
				
				(for-each (lambda (var) (set-binding-rec?! (get-annotation var) #t))
					  vars)
				(zodiac:set-letrec-values-form-vals! ast vals)
				(zodiac:set-letrec-values-form-body! ast body)
				
				(values ast body-multi))))
			  
			  ;;-----------------------------------------------------------
			  ;; POSSIBLY POORLY BEHAVED LETREC
			  ;;   rewrite as let+set!
			  ;;
			  (begin
			    (when (compiler:option:verbose)
			      (compiler:warning ast "letrec will be rewritten with set!"))
			    (debug "rewriting letrec~n")
			    (let ([new-ast (letrec->let+set! ast)])
			      (debug "reanalyzing...~n")
			      (analyze! new-ast env inlined tail? wcm-tail?))))]

		     ;;-----------------------------------------------------
		     ;; IF EXPRESSIONS
		     ;;
		     ;; just analyze the 3 branches.  Very easy
		     [(zodiac:if-form? ast)
		      (zodiac:set-if-form-test! ast (analyze!-sv (zodiac:if-form-test ast) env inlined))
		      (let-values ([(then then-multi) (analyze! (zodiac:if-form-then ast) env inlined tail? wcm-tail?)]
				   [(else else-multi) (analyze! (zodiac:if-form-else ast) env inlined tail? wcm-tail?)])
			(zodiac:set-if-form-then! ast then)
			(zodiac:set-if-form-else! ast else)
			
			(values ast (or-multi then-multi else-multi)))]
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin-form? ast)

		      (let ([last-multi
			     (let loop ([bodies (zodiac:begin-form-bodies ast)])
			       (if (null? (cdr bodies))
				   (let-values ([(e last-multi) (analyze! (car bodies) env inlined tail? wcm-tail?)])
				     (set-car! bodies e)
				     last-multi)
				   (begin
				     (set-car! bodies (analyze!-ast (car bodies) env inlined))
				     (loop (cdr bodies)))))])

			(values ast last-multi))]
		     
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN0 EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin0-form? ast)
		      (let-values ([(0expr 0expr-multi) (analyze! (zodiac:begin0-form-first ast) env inlined #f #f)])
			(zodiac:set-begin0-form-first! ast 0expr)
			(zodiac:set-begin0-form-rest! ast (analyze!-ast (zodiac:begin0-form-rest ast) env inlined))
			(let ([var (get-annotation ast)])
			  (add-local-var! var))
			(values ast 0expr-multi))]
		     
		     ;;--------------------------------------------------------
		     ;; SET! EXPRESSIONS
		     ;;
		     ;; we analyze the target, which will register it as being
		     ;; mutable or used, as necessary.  Then we analyze the value.
		     ;;
		     [(zodiac:set!-form? ast)

		      (let ([target (analyze-varref! (zodiac:set!-form-var ast) env #f #t)])
			(zodiac:set-set!-form-var! ast target)
			(zodiac:set-set!-form-val! 
			 ast 
			 (analyze!-sv (zodiac:set!-form-val ast) env inlined)))
		      
		      (values ast #f)]
		     
		     ;;---------------------------------------------------------
		     ;; DEFINE EXPRESSIONS
		     ;;
		     ;; defines are very tricky, eh what?
		     ;;
		     [(zodiac:define-values-form? ast)

		      (prepare-local-lists)

		      (zodiac:set-define-values-form-vars!
		       ast
		       (map (lambda (v) 
			      (let ([v 
				     ;; Make sure v is not mapped to an import:
				     (ensure-top-level v)])
				(analyze-varref! v env #f #t)))
			    (zodiac:define-values-form-vars ast)))

		      (move-over-local-lists)

		      (zodiac:set-define-values-form-val! 
		       ast
		       (analyze!-ast (zodiac:define-values-form-val ast) env inlined))
		      (values ast #f)]
		     
		     ;;----------------------------------------------------------
		     ;; DEFINE-SYNTAX
		     ;;
		     [(zodiac:define-syntaxes-form? ast)
		      (for-each (lambda (name)
				  (compiler:get-symbol-const! #f (zodiac:varref-var name)))
				(zodiac:define-syntaxes-form-names ast))
		      (zodiac:set-define-syntaxes-form-expr!
		       ast
		       (analyze!-ast (zodiac:define-syntaxes-form-expr ast) env inlined))
		      (values ast #f)]
		     
		     ;;-------------------------------------------------------------------
		     ;; APPLICATIONS
		     ;;   analyze all the parts.  replace with a compiler:app
		     ;;    annotated with tail?
		     ;;  If this is a call to a primitive, check the arity.
		     ;;
		     [(zodiac:app? ast)
		      (check-for-inlining
		       ast
		       env
		       inlined
		       tail?
		       (lambda (new-ast new-inlined)
			 (when (compiler:option:verbose)
			   (compiler:warning ast "inlining procedure call"))
					; We inlined - analyze the new form
			 (analyze! new-ast env new-inlined tail? wcm-tail?))
		       (lambda (why)
			 '(begin
			    (zodiac:print-start! (current-output-port) ast) 
			    (printf "no inlining: ~a~n" (eval why)))
			 (let* ([fun (let ([v (analyze!-sv (zodiac:app-fun ast) env inlined)])
				       (if (zodiac:varref? v)
					   v
					   ;; known non-procedure!
					   (let ([var (zodiac:app-fun ast)])
					     ((if (compiler:option:stupid)
						  compiler:warning 
						  compiler:error)
					      ast 
					      "application of a non-procedure")
					     (if (zodiac:varref? var)
						 (begin
						   (set-binding-known-but-used?! 
						    (get-annotation (zodiac:bound-varref-binding var)) 
						    #t)
						   (analyze-varref! var env #f #t))
						 (analyze!-sv var env inlined)))))]
				[primfun (app-prim-name (get-annotation ast))]
				[multi (if primfun
					   (let ([a (primitive-result-arity 
						     (dynamic-require '#%kernel primfun))])
					     (cond
					      [(and (number? a) (= a 1)) #f]
					      [(number? a) #t]
					      [else 'possible]))
					   'possible)]
				[args (map (lambda (arg)
					     (analyze!-sv arg env inlined))
					   (zodiac:app-args ast))])
			   
					; for all functions, do this stuff
			   (zodiac:set-app-fun! ast fun)
			   (zodiac:set-app-args! ast args)
			   (set-app-tail?! (get-annotation ast) tail?)

			   (register-arity! (length args))

			   (values ast multi))))]
		     
		     ;;-------------------------------------------------------------------
		     ;; WITH-CONTINUATION-MARK
		     ;;
		     ;; analyze the key, val, and body
		     ;;
		     [(zodiac:with-continuation-mark-form? ast)
		      
		      (zodiac:set-with-continuation-mark-form-key!
		       ast
		       (analyze!-sv (zodiac:with-continuation-mark-form-key ast) env inlined))
		      
		      (zodiac:set-with-continuation-mark-form-val!
		       ast
		       (analyze!-sv (zodiac:with-continuation-mark-form-val ast) env inlined))
		      
		      (let-values ([(body body-multi) 
				    (analyze! (zodiac:with-continuation-mark-form-body ast) env inlined tail? #t)])
			
			(if (or tail? wcm-tail?)
					; No frame push, so no need for begin0-like handling
			    (set-annotation! ast #f)
			    
			    (let ([var (get-annotation ast)])
			      (add-local-var! var)))

			(zodiac:set-with-continuation-mark-form-body!
			 ast
			 body)

			(values ast body-multi))]

		     ;;-----------------------------------------------------------
		     ;; GLOBALS
		     ;;
		     [(zodiac:global-prepare? ast)
		      (zodiac:set-global-prepare-vec!
		       ast
		       (analyze!-ast (zodiac:global-prepare-vec ast) env inlined))
		      (values ast #f)]
		     [(zodiac:global-lookup? ast)
		      (zodiac:set-global-lookup-vec!
		       ast
		       (analyze!-ast (zodiac:global-lookup-vec ast) env inlined))
		      (values ast #f)]
		     [(zodiac:global-assign? ast)
		      (zodiac:set-global-assign-vec!
		       ast
		       (analyze!-ast (zodiac:global-assign-vec ast) env inlined))
		      (zodiac:set-global-assign-expr!
		       ast
		       (analyze!-ast (zodiac:global-assign-expr ast) env inlined))
		      (values ast #f)]
		     [(zodiac:safe-vector-ref? ast)
		      (zodiac:set-safe-vector-ref-vec!
		       ast
		       (analyze!-ast (zodiac:safe-vector-ref-vec ast) env inlined))
		      (values ast #f)]

		     [else (compiler:internal-error
			    ast
			    (format "unsupported syntactic form (~a)"
				    (if (struct? ast)
					(vector-ref (struct->vector ast) 0)
					ast)))]))])
	      
	      ;; analyze the expression and return it with the local variables
	      ;; it creates.  
	      (let-values ([(ast multi) (analyze! ast env 0 tail? #f)])
		(values ast
			free-vars
			local-vars
			global-vars
			locals-used
			captured-vars
			codes
			max-arity
			multi))))))))
