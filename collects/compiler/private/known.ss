;; Known-value analysis
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

;; Sets the the real annotation for zodiac:binding AST nodes,
;; setting the known? and known-val fields as possible.

;; Known-value analysis is used for constant propagation, but
;;  more importantly, it's used for compiling tail recursion
;;  as a goto. mzc can only compile tail recursion as a goto
;;  when it knows the actual destination of the jump.

;; Note that ``known'' means we know an AST that provides the
;; value; this AST could be arbitrarily complex, so we really
;; only know the value if the known AST is simple enough.

;;; Annotatitons: ----------------------------------------------
;;    binding - `binding' structure (replaces prephase 
;;      binding-properties structure)
;;    application - `app' structure
;;; ------------------------------------------------------------

(module known mzscheme
  (require (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide known@)
  (define known@
    (unit/sig compiler:known^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:prephase^
	      compiler:anorm^
	      compiler:const^
	      compiler:closure^
	      compiler:rep^
	      compiler:driver^)
      
      ;; helper functions to create a binding annotation
      (define make-known-binding
	(lambda (bound val)
	  (make-binding #f (prephase:is-mutable? bound) 
			'not-unit (prephase:binding-anchor bound)
			#f (prephase:is-ivar? bound)
			#t val #f #f)))
      (define make-unknown-letbound-binding
	(lambda (mutable?)
	  (make-binding #f mutable?
			#f #f
			#f #f
			#f #f #f #f)))
      (define make-unknown-binding
	(lambda (bound)
	  (make-binding #f (prephase:is-mutable? bound) 
			'not-unit (prephase:binding-anchor bound)
			#f (prephase:is-ivar? bound)
			#f #f #f #f)))
      (define make-begin0-binding
	(lambda (bound)
	  (make-binding #f #f #f #f #f #f
			#f #f #f (make-rep:atomic 'begin0-saver))))
      (define make-wcm-binding
	(lambda (bound)
	  (make-binding #f #f #f #f #f #f
			#f #f #f (make-rep:atomic 'wcm-saver))))

      ;; Determine whether a varref is a known primitive
      (define (analyze:prim-fun fun)
	(and (zodiac:top-level-varref? fun)
	     (varref:has-attribute? fun varref:primitive)
	     (primitive? (namespace-variable-value (zodiac:varref-var fun)))
	     (zodiac:varref-var fun)))

      ;; Some prims call given procedures directly, some install procedures
      ;;  to be called later, and some call previously installed procedures.
      ;;  We care abot the installers and callers.
      (define prims-that-induce-procedure-calls
	'(apply map for-each andmap ormap make-promise
		  dynamic-wind thread call-in-nested-thread
		  make-object call-with-values time-apply
		  call-with-output-file call-with-input-file
		  with-output-to-file with-input-from-file
		  exit-handler current-eval current-exception-handler
		  current-prompt-read current-load
		  call-with-escape-continuation call-with-current-continuation
		  current-print port-display-handler port-write-handler
		  port-print-handler global-port-print-handler
		  error-display-handler error-escape-handler
		  port-read-handler error-value->string-handler
		  call/ec call/cc hash-table-get
		  hash-table-map hash-table-for-each make-input-port make-output-port))

      ;; The valueable? predicate is used to determine how many variables
      ;;  are reliably set in a mutually-recursive binding context.
      ;; Along the way, we set more `known-binding' information in let and letrec.
      (define (analyze:valueable? v extra-known-bindings extra-unknown-bindings known-lambdas set-known?)
	(let loop ([v v][extra-known-bindings extra-known-bindings])
	  (cond
	   [(zodiac:quote-form? v) #t]
	   [(zodiac:bound-varref? v)
	    ;; the varref must not be unit-i/e?, or it must be in extra-known-bindings
	    ;; and it must not be in extra-unknown-bindings
	    (let ([zbinding (zodiac:bound-varref-binding v)])
	      (and (not (memq zbinding extra-unknown-bindings))
		   (memq zbinding extra-known-bindings)))]
	   [(zodiac:varref? v) #t]
	   [(zodiac:case-lambda-form? v) #t]
	   [(zodiac:begin-form? v)
	    (andmap (lambda (v) (loop v extra-known-bindings)) (zodiac:begin-form-bodies v))]
	   [(zodiac:begin0-form? v)
	    (andmap (lambda (v) (loop v extra-known-bindings)) (zodiac:begin0-form-bodies v))]
	   [(zodiac:with-continuation-mark-form? v)
	    (and (loop (zodiac:with-continuation-mark-form-key v) extra-known-bindings)
		 (loop (zodiac:with-continuation-mark-form-val v) extra-known-bindings)
		 (loop (zodiac:with-continuation-mark-form-body v) extra-known-bindings))]
	   [(zodiac:set!-form? v) #f] ; because it changes a variable
	   [(zodiac:if-form? v)
	    (and (loop (zodiac:if-form-test v) extra-known-bindings)
		 (loop (zodiac:if-form-then v) extra-known-bindings)
		 (loop (zodiac:if-form-else v) extra-known-bindings))]
	   [(zodiac:let-values-form? v)
	    (and (andmap (lambda (vars v) (if (loop v extra-known-bindings)
					      (begin
						(when (and set-known?
							   (= 1 (length vars)))
						  (prephase:set-known-val! (car vars) v))
						#t)
					      #f))
			 (zodiac:let-values-form-vars v)
			 (zodiac:let-values-form-vals v))
		 (loop (zodiac:let-values-form-body v)
		       (append (apply append (zodiac:let-values-form-vars v))
			       extra-known-bindings)))]
	   [(zodiac:letrec-values-form? v)
	    (and (andmap (lambda (vars v) (if (loop v extra-known-bindings)
					      (begin
						(when (and set-known?
							   (= 1 (length vars)))
						  (prephase:set-known-val! (car vars) v))
						#t)
					      #f))
			 (zodiac:letrec-values-form-vars v)
			 (zodiac:letrec-values-form-vals v))
		 (loop (zodiac:letrec-values-form-body v)
		       (append (apply append (zodiac:letrec-values-form-vars v))
			       extra-known-bindings)))]
	   [(zodiac:app? v)
	    (let* ([fun (zodiac:app-fun v)]
		   [primfun (analyze:prim-fun fun)]
		   [args (zodiac:app-args v)]
		   [args-ok? (lambda ()
			       (andmap (lambda (v) (loop v extra-known-bindings)) 
				       args))])
	      (if primfun

		  ;; Check whether the primitive can call any procedures:
		  (and (not (memq primfun prims-that-induce-procedure-calls))
		       (args-ok?))
		  
		  ;; Interesting special case: call a known function
		  (and (loop fun extra-known-bindings)
		       (args-ok?)
		       (let ([simple-case-lambda?
			      ;; We know nothing about the args; still valueable?
			      ;; What if we're trying to check a recursive function?
			      ;;  If we encounter a cycle, the body is valueable.
			      (lambda (v)
				(or (memq v known-lambdas)
				    (andmap
				     (lambda (body) (analyze:valueable? 
						     body 
						     extra-known-bindings
						     extra-unknown-bindings
						     (cons v known-lambdas)
						     #f))
				     (zodiac:case-lambda-form-bodies v))))])
			 (cond
			  [(zodiac:bound-varref? fun)
			   (let ([v (extract-varref-known-val fun)])
			     (and v 
				  (cond
				   [(zodiac:case-lambda-form? v)
				    (simple-case-lambda? v)]
				   [(zodiac:top-level-varref? v)
				    ;; Could be a friendly primitive...
				    (let ([primfun (analyze:prim-fun v)])
				      (and primfun
					   (not (memq primfun prims-that-induce-procedure-calls))))]
				   [else #f])))]
			  [(zodiac:case-lambda-form? fun) (simple-case-lambda? fun)]
			  [else #f])))))]

	   [else #f])))

      ;; extract-ast-known-value tries to extract a useful value from a known-value AST
      (define (extract-ast-known-value v)
	(let extract-value ([v v])
	  (cond
	   [(zodiac:set!-form? v) (zodiac:make-special-constant 'void)]
	   [(zodiac:begin-form? v) (extract-value (car (last-pair (zodiac:begin-form-bodies v))))]
	   [(zodiac:begin0-form? v) (extract-value (car (zodiac:begin0-form-bodies v)))]
	   [(zodiac:with-continuation-mark-form? v) (extract-value (zodiac:with-continuation-mark-form-body v))]
	   [(zodiac:let-values-form? v) (extract-value (zodiac:let-values-form-body v))]
	   [(zodiac:letrec-values-form? v) (extract-value (zodiac:letrec-values-form-body v))]
	   [(zodiac:app? v)
	    (let ([fun (analyze:prim-fun (zodiac:app-fun v))])
	      (if fun
		  (let ([args (map extract-value (zodiac:app-args v))])
		    (case fun
		      [(void) (zodiac:make-special-constant 'void)]
		      [(char->integer) 
		       (with-handlers ([void (lambda (x) v)])
			 (let ([args (map (lambda (a) (syntax-e (zodiac:zodiac-stx (zodiac:quote-form-expr a)))) args)])
			   (let ([new-v (apply (namespace-variable-value fun) args)])
			     (zodiac:make-quote-form
			      (zodiac:zodiac-stx v)
			      (make-empty-box)
			      (zodiac:structurize-syntax new-v v)))))]
		      [else v]))
		  v))]
	   [(top-level-varref/bind-from-lift? v) (top-level-varref/bind-from-lift-lambda v)]
	   [(zodiac:bound-varref? v) (extract-ast-known-value (extract-varref-known-val v))]
	   [else v])))

      ;; extract-varref-known-val works for bindings, too.
      (define (extract-varref-known-val v)
	(if (top-level-varref/bind-from-lift? v)
	    (top-level-varref/bind-from-lift-lambda v)
	    (let loop ([v v])
	      (let* ([zbinding (if (zodiac:binding? v)
				   v
				   (zodiac:bound-varref-binding v))]
		     [binding (get-annotation zbinding)]
		     [result (lambda (v)
			       (cond
				[(top-level-varref/bind-from-lift? v)
				 (extract-varref-known-val v)]
				[(or (zodiac:bound-varref? v)
				     (zodiac:binding? v))
				 (loop v)]
				[else v]))])
		(and binding
		     (cond
		      [(binding? binding)
		       (and (binding-known? binding)
			    (result (binding-val binding)))]
		      [else
		       (result (prephase:known-val zbinding))]))))))

      ;; analyze-knowns! sets the annotation for binding occurrences, setting information
      ;; about known variables. Also sets the annotation for applications.
      (define analyze-knowns!
	(letrec ([analyze!
		  (lambda (ast)
		    (when (compiler:option:debug)
		      (zodiac:print-start! (debug:get-port) ast)
		      (newline (debug:get-port)))
		    
		    (cond
		     
		     ;;-----------------------------------------------------------------
		     ;; CONSTANTS (A-VALUES)
		     [(zodiac:quote-form? ast) ast]
		     
		     ;;-----------------------------------------------------------------
		     ;; VARIABLE REFERENCES (A-VALUES)
		     ;;
		     [(zodiac:bound-varref? ast) ast]
		     [(zodiac:top-level-varref? ast) ast]
		     
		     ;;--------------------------------------------------------------------
		     ;; LAMBDA EXPRESSIONS
		     ;;    analyze the bodies, and set binding info for the binding vars
		     ;;
		     [(zodiac:case-lambda-form? ast)
		      (zodiac:set-case-lambda-form-bodies! 
		       ast 
		       (map
			(lambda (args body)
			  
			  ;; annotate each binding with our information
			  (for-each
			   (lambda (bound) (set-annotation! bound (make-known-binding bound #f)))
			   (zodiac:arglist-vars args))
			  
			  (analyze! body))
			(zodiac:case-lambda-form-args ast)
			(zodiac:case-lambda-form-bodies ast)))

		      ast]
		     
		     ;;--------------------------------------------------------------
		     ;; LET EXPRESSIONS
		     ;;    Several values may be bound at once, in which case 'known'
		     ;;    analysis is not performed.
		     ;;
		     ;;   Variables are assumed to be immutable and known, unless
		     ;;    proven otherwise; we store this information
		     ;;    in the binding structure in the compiler:bound structure.
		     ;;
		     [(zodiac:let-values-form? ast)
		      (let* ([val (analyze! (car (zodiac:let-values-form-vals ast)))]
			     [vars (car (zodiac:let-values-form-vars ast))]
			     [bindings (map 
					(lambda (var)
					  (make-known-binding var (extract-ast-known-value val)))
					vars)])
			
			(for-each set-annotation! vars bindings)
			(set-car! (zodiac:let-values-form-vals ast) val)
			
			(if (= 1 (length vars))
			    
					; this is a one-value binding let
			    (let* ([var (car vars)])
			      
			      (when (binding-mutable? (car bindings))
				(set-binding-known?! (car bindings) #f)))
			    
					; this is a multiple (or zero) value binding let
					; the values are unknown to simple analysis so skip
					; that stuff;
					; nothing is known
			    (for-each (lambda (binding) (set-binding-known?! binding #f))
				      bindings))

					; analyze the body
			(let ([body (analyze! (zodiac:let-values-form-body ast))])
			  (zodiac:set-let-values-form-body! ast body)))
		      
		      ast]

		     ;;-----------------------------------------------------------------
		     ;; LETREC EXPRESSIONS
		     ;;
		     [(zodiac:letrec-values-form? ast)
		      
		      (let* ([varses (zodiac:letrec-values-form-vars ast)]
			     [vals (zodiac:letrec-values-form-vals ast)])
			
					; Annotate each binding occurrence
			(for-each
			 (lambda (vars)
			   (for-each (lambda (var)
				       (let ([binding (make-unknown-binding var)])
					 (set-annotation! var binding)))
				     vars))
			 varses)
			
					; Mark known letrec-bound vars
			(let loop ([varses varses][vals vals][done-vars null])
			  (unless (null? vals)
			    (when (analyze:valueable? (car vals) done-vars (apply append varses) null #t)
			      
					; Continue known marking
			      (let ([vars (car varses)])
				(when (= 1 (length vars))
				  (let ([binding (get-annotation (car vars))])
				    (unless (binding-mutable? binding)
				      (set-binding-known?! binding #t)
				      (set-binding-val! binding (car vals)))))
				(loop (cdr varses) (cdr vals)
				      (append vars done-vars))))))

			(zodiac:set-letrec-values-form-vals! ast (map analyze! vals))
			
			(zodiac:set-letrec-values-form-body!
			 ast
			 (analyze! (zodiac:letrec-values-form-body ast)))
			
			ast)]
		     
		     ;;-----------------------------------------------------
		     ;; IF EXPRESSIONS
		     ;;
		     ;;  analyze the 3 branches.
		     ;;
		     [(zodiac:if-form? ast)
		      (zodiac:set-if-form-test! ast (analyze! (zodiac:if-form-test ast)))
		      (let ([then (analyze! (zodiac:if-form-then ast))]
			    [else (analyze! (zodiac:if-form-else ast))])
			(zodiac:set-if-form-then! ast then)
			(zodiac:set-if-form-else! ast else)
			
			ast)]
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin-form? ast)
		      
		      (let loop ([bodies (zodiac:begin-form-bodies ast)])
			(if (null? (cdr bodies))
			    (let ([e (analyze! (car bodies))])
			      (set-car! bodies e))
			    (begin
			      (set-car! bodies (analyze! (car bodies)))
			      (loop (cdr bodies)))))
		      
		      ast]
		     
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN0 EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin0-form? ast)
		      (zodiac:set-begin0-form-first! ast (analyze! (zodiac:begin0-form-first ast)))
		      (zodiac:set-begin0-form-rest! ast (analyze! (zodiac:begin0-form-rest ast)))
		      (let ([var (get-annotation ast)])
			(set-annotation! var (make-begin0-binding var)))
		      ast]
		     
		     ;;--------------------------------------------------------
		     ;; SET! EXPRESSIONS
		     ;;
		     ;; we analyze the target, which will register it as being
		     ;; mutable or used, as necessary.  Then we analyze the value.
		     ;;
		     [(zodiac:set!-form? ast)

		      (let ([target (analyze! (zodiac:set!-form-var ast))])
			(when (zodiac:bound-varref? target)
			  (let ([binding (compiler:bound-varref->binding target)])
			    (unless (binding-mutable? binding)
			      (compiler:internal-error 
			       target 
			       (string-append
				"analyze: variable found in set! but not"
				" marked mutable by prephase!")))
			    (when (binding-mutable? binding)
			      (set-binding-known?! binding #f))))
			(zodiac:set-set!-form-var! ast target)
			(zodiac:set-set!-form-val! 
			 ast 
			 (analyze! (zodiac:set!-form-val ast))))
		      
		      ast]
		     
		     ;;---------------------------------------------------------
		     ;; DEFINE EXPRESSIONS
		     ;;
		     [(zodiac:define-values-form? ast)
		      (zodiac:set-define-values-form-vars!
		       ast
		       (map (lambda (v) (analyze! v))
			    (zodiac:define-values-form-vars ast)))
		      (zodiac:set-define-values-form-val! 
		       ast
		       (analyze! (zodiac:define-values-form-val ast)))
		      ast]
		     
		     ;;----------------------------------------------------------
		     ;; DEFINE-SYNTAX
		     ;;
		     [(zodiac:define-syntaxes-form? ast)
		      (zodiac:set-define-syntaxes-form-expr!
		       ast
		       (analyze! (zodiac:define-syntaxes-form-expr ast)))
		      ast]
		     
		     ;;-------------------------------------------------------------------
		     ;; APPLICATIONS
		     ;;  analyze all the parts, and note whether the rator is
		     ;;  a primitive;
		     ;;  if this is a call to a primitive, check the arity.
		     ;;
		     [(zodiac:app? ast)
		      
		      (let* ([fun (analyze! (zodiac:app-fun ast))]
			     [args (map (lambda (arg) (analyze! arg))
					(zodiac:app-args ast))]
			     [primfun (analyze:prim-fun fun)]
			     [primfun-arity-ok?
			      ;; check the arity for primitive apps -- just an error check
			      (and primfun
				   (let* ([num-args (length args)]
					  [arity-ok? (procedure-arity-includes?
						      (namespace-variable-value primfun)
						      num-args)])
				     (unless arity-ok?
				       ((if (compiler:option:stupid)
					    compiler:warning
					    compiler:error)
					ast
					(format "~a got ~a argument~a"
						(zodiac:varref-var fun)
						num-args
						(if (= num-args 1)
						    ""
						    "s"))))
				     arity-ok?))]
			     [prim? (and primfun primfun-arity-ok?)])
			
					; for all functions, do this stuff
			(zodiac:set-app-fun! ast fun)
			(zodiac:set-app-args! ast args)
			(set-annotation! 
			 ast 
			 (make-app #f prim? (and prim? primfun)))
			
			ast)]
		     
		     ;;-------------------------------------------------------------------
		     ;; WITH-CONTINUATION-MARK
		     ;;
		     ;; analyze the key, val, and body, and the binding in the annotation
		     ;;
		     [(zodiac:with-continuation-mark-form? ast)
		      
		      (zodiac:set-with-continuation-mark-form-key!
		       ast
		       (analyze! (zodiac:with-continuation-mark-form-key ast)))
		      
		      (zodiac:set-with-continuation-mark-form-val!
		       ast
		       (analyze! (zodiac:with-continuation-mark-form-val ast)))
		      
		      (zodiac:set-with-continuation-mark-form-body!
		       ast
		       (analyze! (zodiac:with-continuation-mark-form-body ast)))

		      (let ([var (get-annotation ast)])
			(set-annotation! var (make-wcm-binding var)))
		      
		      ast]

		     ;;-----------------------------------------------------------------
		     ;; QUOTE-SYNTAX
		     ;;
		     ;; Construct constant.
		     ;;
		     [(zodiac:quote-syntax-form? ast)
		      ast]
		     
		     ;;-----------------------------------------------------------
		     ;; MODULE
		     ;;
		     [(zodiac:module-form? ast)

		      (zodiac:set-module-form-body!
		       ast
		       (analyze! (zodiac:module-form-body ast)))

		      ast]
		     

		     [else (compiler:internal-error
			    ast
			    (format "unsupported syntactic form (~a)"
				    (if (struct? ast)
					(vector-ref (struct->vector ast) 0)
					ast)))]))])
	  
	  (lambda (ast)
	    (analyze! ast)))))))
