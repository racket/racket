;; pre-compilation scan
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

; Notes mutability of lexical variables.
; Performs a few very-high-level optimizations, such as
;  throwing away constant expressions in a begin.
; Performs a few ad hoc optimizations, like (+ x 1)
;  => (add1 x)
; Normalizes the expression forms:
;   - begin/begin0: flattened as much as possible; empty
;     and one-expression begins are eliminated
;   - ((lambda (x1 ... xn) e) a1 ... an1) => let expression
;   - (define-values () e) => (let-values [() e] (void))
; (After this phase, a zodiac:top-level-varref is always
;  a global variable.)
; Infers names for closures and interfaces. (Do this early so
;  that elaboration doesn't mangle the names.)
; Detects global varrefs to built-in primitives.
; Detects known immutability of signature vectors produced by */sig
;  forms
; Lambdas that are really c-lambdas are converted to quote forms
;  containing c-lambda records
; Applications that are really c-declares are converted to voids
; Converts define-for-syntax to define-syntaxes (where id module
;  phase distinguishes them in the end)

;;; Annotatitons: ----------------------------------------------
;;    binding - `binding-properties' structure
;;        (this is temporary; the next phase will change the
;;         annotation)
;;    varref - empty set of varref attrributes, except that
;;        the varref:primitive attribute can be added, and
;;        the varref:in-module attribute can be added
;;    quote - 'immutable for known immutable quoted vars
;;    lambda - an inferred name (temporary)
;;    module - a module-info record
;;    define-[for-]syntax - in-mod?, wrap RHS with 
;;                          (for-syntax-in-env (lambda () ...))
;;; ------------------------------------------------------------

(module prephase mzscheme
  (require (lib "unit.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide prephase@)
  (define-unit prephase@
      (import (prefix compiler:option: compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (prefix zodiac: zodiac^)
	      compiler:zlayer^
	      compiler:driver^)
      (export compiler:prephase^)

      (define-struct binding-properties (mutable? unit-i/e? ivar? anchor known-val))

      (define (prephase:init-binding-properties! binding mutable? unit-i/e? ivar?)
	(set-annotation! binding (make-binding-properties mutable? unit-i/e? ivar? #f #f)))

      (define (prephase:set-mutable! binding mutable?)
	(set-binding-properties-mutable?! (get-annotation binding) mutable?))
      (define (prephase:set-binding-anchor! binding a)
	(set-binding-properties-anchor! (get-annotation binding) a))

      (define (prephase:is-mutable? binding)
	(let ([p (get-annotation binding)])
	  (and p (binding-properties-mutable? p))))
      (define (prephase:is-ivar? binding)
	(let ([p (get-annotation binding)])
	  (and p (binding-properties-ivar? p))))
      (define (prephase:binding-anchor binding)
	(let ([p (get-annotation binding)])
	  (and p (binding-properties-anchor p))))

      ;; Used in analyze to temporarily store known-value information for
      ;;  let[rec] bindings
      (define (prephase:known-val binding)
	(let ([p (get-annotation binding)])
	  (and p (binding-properties-known-val p))))
      (define (prephase:set-known-val! binding v)
	(let ([p (get-annotation binding)])
	  (if p
	      (set-binding-properties-known-val! p v)
	      (begin
		(prephase:init-binding-properties! binding #f #f #f)
		(prephase:set-known-val! binding v)))))

      ;; what can be thrown away in a begin?
      (define prephase:dead-expression?
	(one-of zodiac:bound-varref? zodiac:quote-form?
		zodiac:case-lambda-form?))

      ;; what can be ``pushed''?: (begin0 x ...) => (begin ... x)
      (define prephase:begin0-pushable?
	(one-of zodiac:case-lambda-form? zodiac:quote-form?))

      ;; returns a true value if the symbol refers to a primitive function.
      (define prephase:primitive-name?
	(lambda (ast)
	  (let ([m (zodiac:top-level-varref-module ast)])
	    (or (eq? '#%kernel m)
		(and (box? m)
		     (eq? '#%kernel (unbox m)))))))

      (define (preprocess:adhoc-app-optimization ast prephase-it)
	(let ([fun (zodiac:app-fun ast)])
	  (and (zodiac:top-level-varref? fun)
	       (prephase:primitive-name? fun)
	       (let ([name (zodiac:varref-var fun)]
		     [args (zodiac:app-args ast)]
		     [new-fun (lambda (newname)
				(prephase-it
				 (zodiac:make-top-level-varref
				  ;; FIXME?: wrong syntax
				  (zodiac:zodiac-stx fun)
				  (make-empty-box)
				  newname
				  '#%kernel
				  (box '())
				  #f
				  #f
				  #f)))])
		 (case name
		   [(void) (if (null? args)
			       (prephase-it (zodiac:make-special-constant 'void))
			       #f)]
		   [(list) (if (null? args)
			       (prephase-it (zodiac:make-special-constant 'null))
			       #f)]
		   [(+ -) (when (and (= 2 (length args))
				     (zodiac:quote-form? (cadr args))
				     (equal? 1 (syntax-e (zodiac:zodiac-stx (zodiac:quote-form-expr (cadr args))))))
			    (let ([newname (if (eq? name '+) 'add1 'sub1)])
			      (zodiac:set-app-fun! ast (new-fun newname))
			      (zodiac:set-app-args! ast (list (car args)))))
		    #f] ; always return #f => use the (possibly mutated) ast
		   [(verify-linkage-signature-match)
		    ;; Important optimization for compound-unit/sig: mark signature-defining vectors
		    ;;  as immutable
		    (when (= 5 (length args))
		      ;; Mark 1st, 2nd, 4th, and 5th as 'immutable quotes
		      (let ([mark (lambda (qf)
				    (when (zodiac:quote-form? qf)
				      (set-annotation! qf 'immutable)))])
			(mark (list-ref args 0))
			(mark (list-ref args 1))
			(mark (list-ref args 3))
			(mark (list-ref args 4))))
		    #f]
		   [else #f])))))

      (define for-syntax-slot (box #f))

      ;;----------------------------------------------------------------------------
      ;; PREPHASE MAIN FUNCTION
      ;;
      (define prephase!
	(letrec ([prephase!
		  (lambda (ast in-mod? need-val? name)
		    (when (compiler:option:debug)
		      (zodiac:print-start! (debug:get-port) ast)
		      (newline (debug:get-port)))
		    (cond
		     ;;----------------------------------------------------------
		     ;; CONSTANTS
		     ;;
		     [(zodiac:quote-form? ast) ast]
		     
		     ;;----------------------------------------------------------
		     ;; VARIABLE REFERENCES
		     ;;
		     ;; set up all varrefs with an attribute set
		     ;; note all varrefs to primitives
		     ;; change unit-bound `top-levels' to lexicals
		     ;;
		     [(zodiac:varref? ast)

		      (set-annotation! ast (varref:empty-attributes))
			    
		      (when (zodiac:top-level-varref? ast)
			(when (prephase:primitive-name? ast)
			  (varref:add-attribute! ast varref:primitive))
			(when in-mod?
			  (varref:add-attribute! ast varref:in-module)))
		      
		      ast]
		     
		     ;;----------------------------------------------------------
		     ;; LAMBDA EXPRESSIONS
		     ;;
		     [(zodiac:case-lambda-form? ast)
		      ;; Check for 'mzc-cffi attribute:
		      (if (syntax-property (zodiac:zodiac-stx ast) 'mzc-cffi)

			  ;; A C glue function. Change to a quote so it gets treated atomically
			  (let* ([quote-expr (cadr
					      (zodiac:begin-form-bodies (car (zodiac:case-lambda-form-bodies ast))))]
				 [elems (syntax-e (zodiac:zodiac-stx (zodiac:quote-form-expr quote-expr)))]
				 [fname (syntax-e (car elems))]
				 [sname (syntax-e (cadr elems))]
				 [arity (syntax-e (caddr elems))]
				 [body (syntax-e (cadddr elems))])
			    (register-c-lambda-function fname body)
			    (zodiac:make-quote-form
			     (zodiac:zodiac-stx ast)
			     (zodiac:parsed-back ast)
			     (zodiac:make-zread
			      (datum->syntax-object
			       #f
			       (make-c-lambda fname sname body arity)
			       #f))))
			  
			  ;; Normal lambda
			  (let ([args (zodiac:case-lambda-form-args ast)]
				[bodies (zodiac:case-lambda-form-bodies ast)])
			    (for-each
			     (lambda (args)
			       (for-each (lambda (b) (prephase:init-binding-properties! b #f #f #f))
					 (zodiac:arglist-vars args)))
			     args)
			    (let ([ast (zodiac:make-case-lambda-form
					(zodiac:zodiac-stx ast)
					(zodiac:parsed-back ast)
					args
					(begin-map (lambda (e) (prephase! e in-mod? #f #f))
						   (lambda (e) (prephase! e in-mod? #t #f))
						   bodies))])
			      (set-annotation! ast name)
			      ast)))]
		     
		     ;;----------------------------------------------------------
		     ;; LET EXPRESSIONS
		     ;;
		     [(zodiac:let-values-form? ast)
		      (for-each 
		       (lambda (l)
			 (for-each (lambda (b) (prephase:init-binding-properties! b #f #f #f))
				   l))
		       (zodiac:let-values-form-vars ast))
		      (zodiac:set-let-values-form-vals!
		       ast
		       (map (lambda (e name) (prephase! e in-mod? #t name))
			    (zodiac:let-values-form-vals ast)
			    (zodiac:let-values-form-vars ast)))
		      (zodiac:set-let-values-form-body!
		       ast
		       (prephase! (zodiac:let-values-form-body ast) in-mod? need-val? name))
		      ast]
		     
		     ;;-----------------------------------------------------------
		     ;; LETREC EXPRESSIONS
		     ;;
		     [(zodiac:letrec-values-form? ast)
		      (for-each (lambda (l)
				  (for-each (lambda (b) 
					      (prephase:init-binding-properties! b #f #f #f))
					    l))
				(zodiac:letrec-values-form-vars ast))
		      (zodiac:set-letrec-values-form-vals!
		       ast
		       (map (lambda (e name) (prephase! e in-mod? #t name))
			    (zodiac:letrec-values-form-vals ast)
			    (zodiac:letrec-values-form-vars ast)))
		      (zodiac:set-letrec-values-form-body!
		       ast
		       (prephase! 
			(zodiac:letrec-values-form-body ast)
			in-mod?
			need-val?
			name))

		      ;; ????? Obsolete? ????
		      ;; this will mark the letrec so it is NOT retraversed by
		      ;; a possible future call to a-normalize! (the mutating version)
		      ;; (set-annotation! ast #f)

		      ast]
		     
		     ;;-----------------------------------------------------------
		     ;; IF EXPRESSIONS
		     ;;
		     [(zodiac:if-form? ast)
		      (zodiac:set-if-form-test! 
		       ast
		       (prephase! (zodiac:if-form-test ast) in-mod? #t #f))
		      (zodiac:set-if-form-then!
		       ast
		       (prephase! (zodiac:if-form-then ast) in-mod? need-val? name))
		      (zodiac:set-if-form-else!
		       ast
		       (prephase! (zodiac:if-form-else ast) in-mod? need-val? name))

		      ;; Ad hoc optimization: (if (not x) y z) => (if x z y)
		      (let ([test (zodiac:if-form-test ast)])
			(when (and (zodiac:app? test)
				   (zodiac:top-level-varref? (zodiac:app-fun test))
				   (eq? 'not (zodiac:varref-var (zodiac:app-fun test)))
				   (prephase:primitive-name? (zodiac:app-fun test))
				   (= 1 (length (zodiac:app-args test))))
			  (let ([then (zodiac:if-form-then ast)]
				[else (zodiac:if-form-else ast)])
			    (zodiac:set-if-form-test! ast (car (zodiac:app-args test)))
			    (zodiac:set-if-form-then! ast else)
			    (zodiac:set-if-form-else! ast then))))

		      ast]
		     
		     ;;-----------------------------------------------------------
		     ;; BEGIN EXPRESSIONS
		     ;; 
		     ;; flatten, throw away dead values
		     ;;
		     [(zodiac:begin-form? ast)
		      (let ([bodies (zodiac:begin-form-bodies ast)])
			(if (null? bodies)
					; must be a top-level begin...
			    (zodiac:make-special-constant 'void)

					; Normal begin
			    (begin
			      (begin-map! (lambda (e) (prephase! e in-mod? #f #f))
					  (lambda (e) (prephase! e in-mod? need-val? name))
					  bodies)
			      (let ([final-bodies
				     (let loop ([bodies bodies])
				       (cond
					; last expr in begin, finished
					[(null? (cdr bodies)) bodies]
					
					; flatten begins
					[(zodiac:begin-form? (car bodies))
					 (loop (append! (zodiac:begin-form-bodies (car bodies))
							(cdr bodies)))]
					
					; flatten begin0s, too
					[(zodiac:begin0-form? (car bodies))
					 (loop (append! (zodiac:begin0-form-bodies (car bodies))
							(cdr bodies)))]
					
					; throw away dead values if possible
					[(prephase:dead-expression? (car bodies))
					 (loop (cdr bodies))]
					
					; otherwise
					[else (cons (car bodies) (loop (cdr bodies)))]))])
				(if (null? (cdr final-bodies))
				    (car final-bodies)
				    (begin
				      (zodiac:set-begin-form-bodies! ast final-bodies)
				      ast))))))]

		     ;;-----------------------------------------------------------
		     ;; BEGIN0 EXPRESSIONS
		     ;; 
		     ;; the 1st place is special -- the rest is just a begin
		     ;; do our begin rewrites, then transform to a general form
		     ;; if necessary
		     ;;
		     ;; if the value isn't going to be used, then the whole thing
		     ;; is a begin
		     ;;
		     [(zodiac:begin0-form? ast)
		      (if (not need-val?)

			  ;; The value is ignored anyway - make it a begin
			  (prephase!
			   (zodiac:make-begin-form (zodiac:zodiac-stx ast)
						   (zodiac:parsed-back ast)
						   (zodiac:begin0-form-bodies ast))
			   in-mod? 
			   #f
			   #f)

			  (let ([ast
				 (let ([make-begin
					(lambda (bodies)
					  (zodiac:make-begin-form (zodiac:zodiac-stx ast)
								  (zodiac:parsed-back ast)
								  bodies))]
				       [bodies (zodiac:begin0-form-bodies ast)])
				   
					; simplify the first position
				   (set-car! bodies (prephase! (car bodies) in-mod? need-val? name))
				   
					; then simplify the begin0
				   (cond
				    
					; (begin0 M) --> M
				    [(null? (cdr bodies)) (car bodies)]
				    
					; (begin0 <push> ...) --> (begin ... <push>))
				    [(prephase:begin0-pushable? (car bodies))
				     (prephase! 
				      (make-begin (append (cdr bodies) (list (car bodies))))
				      in-mod? 
				      need-val?
				      name)]
				    
					; (begin0 M ...) --> (begin0 M (begin ...))
				    [else
				     (set-cdr!
				      (zodiac:begin0-form-bodies ast)
				      (list (prephase! (make-begin (cdr bodies)) in-mod? #f #f)))
				     ast]
				    
				    ))])
			    (if (zodiac:begin0-form? ast)
				ast ; (prephase:convert-begin0 ast)
				ast)))]
		     
		     

		     
		     ;;-----------------------------------------------------------
		     ;; SET! EXPRESSIONS
		     ;;
		     ;; Mark lexical bindings as mutable
		     ;;
		     [(zodiac:set!-form? ast)
		      (zodiac:set-set!-form-var! ast
						 (prephase!
						  (zodiac:set!-form-var ast)
						  in-mod? 
						  #t
						  #f))
		      (let ([target (zodiac:set!-form-var ast)])
			(when (zodiac:bound-varref? target)
			  (prephase:set-mutable! 
			   (zodiac:bound-varref-binding target) #t))
			
			(zodiac:set-set!-form-val! ast 
						   (prephase! 
						    (zodiac:set!-form-val ast)
						    in-mod? 
						    #t
						    (zodiac:set!-form-var ast)))
			ast)]		  
		     
		     ;;-----------------------------------------------------------
		     ;; DEFINE EXPRESSIONS
		     ;;
		     ;;
		     [(zodiac:define-values-form? ast)

		      (if (null? (zodiac:define-values-form-vars ast))

			  ;; (define-values () e) => (let-values [() e] (void))
			  (zodiac:make-let-values-form 
			   (zodiac:zodiac-stx ast)
			   (zodiac:parsed-back ast)
			   (list null)
			   (list (prephase! (zodiac:define-values-form-val ast) in-mod? #t #f))
			   (zodiac:make-special-constant 'void))
			  
			  ;; Normal prephase
			  (begin
			    (zodiac:set-define-values-form-vars!
			     ast
			     (map (lambda (e) (prephase! e in-mod? #t #f))
				  (zodiac:define-values-form-vars ast)))
			    (zodiac:set-define-values-form-val!
			     ast
			     (prephase! (zodiac:define-values-form-val ast) in-mod? #t (zodiac:define-values-form-vars ast)))
			    ast))]
		     
		     ;;----------------------------------------------------------
		     ;; DEFINE-SYNTAX or DEFINE-FOR-SYNTAX
		     ;;
		     [(or (zodiac:define-syntaxes-form? ast)
			  (zodiac:define-for-syntax-form? ast))
		      (let ([get-names (if (zodiac:define-for-syntax-form? ast)
					   zodiac:define-for-syntax-form-names
					   zodiac:define-syntaxes-form-names)]
			    [get-expr (if (zodiac:define-for-syntax-form? ast)
					  zodiac:define-for-syntax-form-expr
					  zodiac:define-syntaxes-form-expr)])
			(let ([ast
			       (zodiac:make-define-syntaxes-form 
				(zodiac:zodiac-stx ast)
				(zodiac:parsed-back ast)
				(map (lambda (e) (prephase! e in-mod? #t #f))
				     (get-names ast))
				(prephase! (zodiac:make-app 
					    (zodiac:zodiac-stx ast)
					    (make-empty-box)
					    (zodiac:make-top-level-varref
					     for-syntax-in-env-stx
					     (make-empty-box)
					     'for-syntax-in-env
					     #f
					     for-syntax-slot
					     #t
					     #f
					     #f)
					    (list
					     (zodiac:make-case-lambda-form
					      (zodiac:zodiac-stx ast)
					      (make-empty-box)
					      (list (zodiac:make-list-arglist null))
					      (list (get-expr ast)))))
					   in-mod?
					   #t (get-names ast)))])
			  (set-annotation! ast in-mod?)
			  ast))]
		     
		     ;;-----------------------------------------------------------
		     ;; APPLICATIONS
		     ;;
		     ;; check for unsupported syntactic forms that end up
		     ;; looking like applications
		     ;;
		     ;; We'll hack in a rewrite here that turns
		     ;; ((lambda (x*) M) y*) -> (let ([x y]*) M)
		     ;;
		     [(zodiac:app? ast)
		      ;; Check for 'mzc-cffi attribute:
		      (if (syntax-property (zodiac:zodiac-stx ast) 'mzc-cffi)
			  
			  ;; Really a c-declare
			  (let* ([quote-expr (caddr (zodiac:app-args ast))]
				 [str (syntax-e (zodiac:zodiac-stx (zodiac:quote-form-expr quote-expr)))])
			    (register-c-declaration str)
			    ;; return a void
			    (zodiac:make-quote-form (zodiac:zodiac-stx ast)
						    (make-empty-box)
						    (zodiac:make-zread
						     (datum->syntax-object #f (void) #f))))
			  
			  (let ([process-normally
				 (lambda ()
				   (zodiac:set-app-fun!
				    ast
				    (prephase! (zodiac:app-fun ast) in-mod? #t #f))
				   (let ([adhoc (preprocess:adhoc-app-optimization 
						 ast
						 (lambda (x)
						   (prephase! x in-mod? #t #f)))])
				     (if adhoc
					 (prephase! adhoc in-mod? need-val? name)
					 (begin
					   (zodiac:set-app-args!
					    ast
					    (map (lambda (e) (prephase! e in-mod? #t #f))
						 (zodiac:app-args ast)))
					   ast))))])
			    
			    (if (and (zodiac:case-lambda-form? (zodiac:app-fun ast))
				     (not (syntax-property (zodiac:zodiac-stx (zodiac:app-fun ast)) 'mzc-cffi))
				     (= 1 (length (zodiac:case-lambda-form-args 
						   (zodiac:app-fun ast))))
				     (zodiac:list-arglist? 
				      (car (zodiac:case-lambda-form-args
					    (zodiac:app-fun ast)))))
				
				;; optimize to let
				(let* ([L (zodiac:app-fun ast)]
				       [args (zodiac:app-args ast)]
				       [ids (zodiac:arglist-vars 
					     (car (zodiac:case-lambda-form-args L)))]
				       [body (car (zodiac:case-lambda-form-bodies L))]
				       [ok? (= (length ids) (length args))])
				  (unless ok?
				    ((if (compiler:option:stupid) compiler:warning compiler:error)
				     ast 
				     "wrong number of arguments to literal function"))
				  (if (not ok?)
				      (process-normally)
				      (prephase!
				       (zodiac:make-let-values-form
					(zodiac:zodiac-stx ast)
					(zodiac:parsed-back ast)
					(map list ids)
					args
					body)
				       in-mod? 
				       need-val?
				       name)))
				
				;; don't optimize
				(process-normally))))]
		     
		     ;;-----------------------------------------------------------
		     ;; WITH-CONTINUATION-MARK
		     ;;
		     [(zodiac:with-continuation-mark-form? ast)
		      
		      (zodiac:set-with-continuation-mark-form-key!
		       ast
		       (prephase! (zodiac:with-continuation-mark-form-key ast) in-mod? #t #f))
		      
		      (zodiac:set-with-continuation-mark-form-val!
		       ast
		       (prephase! (zodiac:with-continuation-mark-form-val ast) in-mod? #t #f))
		      
		      (zodiac:set-with-continuation-mark-form-body!
		       ast
		       (prephase! (zodiac:with-continuation-mark-form-body ast) in-mod? need-val? name))
		      
		      ast]

		     ;;-----------------------------------------------------------
		     ;; REQUIRE/PROVIDE
		     ;;
		     [(zodiac:require/provide-form? ast)
		      ;; Change to namespace[-transformer]-require calls:
		      (let-values ([(elems proc)
				    (syntax-case (zodiac:zodiac-stx ast) (require require-for-syntax)
				      [(require . elem)
				       (values (syntax->list (syntax elem))
					       'namespace-require)]
				      [(require-for-syntax . elem)
				       (values (syntax->list (syntax elem))
					       'namespace-transformer-require)])])
			(let ([proc (zodiac:make-top-level-varref
				     (datum->syntax-object
				      #f
				      'namespace-require
				      (zodiac:zodiac-stx ast))
				     (make-empty-box)
				     proc
				     '#%kernel
				     (box '())
				     #f
				     #f
				     #f)])

			  (prephase!
			   (zodiac:make-begin-form
			    (zodiac:zodiac-stx ast)
			    (make-empty-box)
			    (map (lambda (elem)
				   (zodiac:make-app
				    (zodiac:zodiac-stx ast)
				    (make-empty-box)
				    proc
				    (list (zodiac:make-quote-form
					   (zodiac:zodiac-stx ast)
					   (make-empty-box)
					   (zodiac:make-zread
					    elem)))))
				 elems))
			   in-mod? need-val? name)))]

		     ;;-----------------------------------------------------------
		     ;; QUOTE-SYNTAX
		     ;;
		     [(zodiac:quote-syntax-form? ast)
		      ast]

		     ;;-----------------------------------------------------------
		     ;; GLOBALS
		     ;;
		     [(zodiac:global-prepare? ast)
		      (zodiac:set-global-prepare-vec!
		       ast
		       (prephase! (zodiac:global-prepare-vec ast) in-mod? #t #f))
		      ast]
		     [(zodiac:global-lookup? ast)
		      (zodiac:set-global-lookup-vec!
		       ast
		       (prephase! (zodiac:global-lookup-vec ast) in-mod? #t #f))
		      ast]
		     [(zodiac:global-assign? ast)
		      (zodiac:set-global-assign-vec!
		       ast
		       (prephase! (zodiac:global-assign-vec ast) in-mod? #t #f))
		      (zodiac:set-global-assign-expr!
		       ast
		       (prephase! (zodiac:global-assign-expr ast) in-mod? #t #f))
		      ast]
		     [(zodiac:safe-vector-ref? ast)
		      (zodiac:set-safe-vector-ref-vec!
		       ast
		       (prephase! (zodiac:safe-vector-ref-vec ast) in-mod? #t #f))
		      ast]

		     ;;-----------------------------------------------------------
		     ;; Unsupported forms
		     ;;
		     [else (compiler:fatal-error 
			    ast 
			    (format "unsupported syntactic form ~a" ast))
			   ast]))])
	  prephase!))))
