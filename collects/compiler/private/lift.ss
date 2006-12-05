;; A kind of lambda-lifting
;; (c) 1997-2001 PLT

;; Finds liftable procedures, sets their `liftable' field, and
;;  replaces lexical variables for liftable procedures with
;;  globals variables. This transformation can be applied
;;  anytime (and multiple times) after analyze.ss and before
;;  closure.ss.
;; Liftable lambdas are procedures whose free variables include only
;;  lexical variables bound to known-lifted procedures, primitive
;;  globals, and statics. (No namespace-sentsitive globals and no
;;  per-load statics.) Per-load liftable lambdas are lifted to per-load
;;  allocation; per-load liftable lambdas can have other per-load
;;  liftable lambdas and values in their closure.

;; TODO: get rid of let{rec}-bound variables that are useless
;;  because they are bound to lifted procedures.

;;; Annotatitons: ----------------------------------------------
;;    lambda - sets `liftable' in the procedure-code structure
;;; ------------------------------------------------------------

(module lift mzscheme
  (require (lib "unit.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide lift@)
  (define-unit lift@
      (import (prefix compiler:option: compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (prefix zodiac: zodiac^)
	      compiler:zlayer^
	      compiler:known^
	      compiler:top-level^
	      compiler:analyze^
	      compiler:const^
	      compiler:closure^
	      compiler:driver^)
      (export compiler:lift^)

      (define lifting-allowed? #t)
      (define mutual-lifting-allowed? #t)
      (define per-load-lifting-only? #f)

      (define procedures null)

      (define single-module-mode? #f)
      (define (set-single-module-mode! v)
	(set! single-module-mode? v))

      ;; find-all-procedures!
      (define find-all-procedures!
	(letrec ([find!
		  (lambda (ast)
		    (cond
		     ;;-----------------------------------------------------------------
		     ;; CONSTANTS (A-VALUES)
		     [(zodiac:quote-form? ast) (void)]
		     
		     ;;-----------------------------------------------------------------
		     ;; VARIABLE REFERENCES (A-VALUES)
		     ;;
		     [(zodiac:bound-varref? ast) (void)]

		     [(zodiac:top-level-varref? ast) (void)]
		     
		     ;;--------------------------------------------------------------------
		     ;; LAMBDA EXPRESSIONS
		     ;;
		     [(zodiac:case-lambda-form? ast)
		      (set! procedures (cons (cons ast #f) procedures))
		      (for-each find! (zodiac:case-lambda-form-bodies ast))]
		     
		     ;;--------------------------------------------------------------
		     ;; LET EXPRESSIONS
		     [(zodiac:let-values-form? ast)
		      (find! (car (zodiac:let-values-form-vals ast)))
		      (find! (zodiac:let-values-form-body ast))]

		     ;;-----------------------------------------------------------------
		     ;; LETREC EXPRESSIONS
		     ;;
		     [(zodiac:letrec-values-form? ast)
		      (for-each find! (zodiac:letrec-values-form-vals ast))
		      (find! (zodiac:letrec-values-form-body ast))]
		     
		     ;;-----------------------------------------------------
		     ;; IF EXPRESSIONS
		     ;;
		     ;;  analyze the 3 branches.
		     ;;
		     [(zodiac:if-form? ast)
		      (find! (zodiac:if-form-test ast))
		      (find! (zodiac:if-form-then ast))
		      (find! (zodiac:if-form-else ast))]

		     ;;--------------------------------------------------------
		     ;; BEGIN EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin-form? ast)
		      (for-each find! (zodiac:begin-form-bodies ast))]
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN0 EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin0-form? ast)
		      (find! (zodiac:begin0-form-first ast))
		      (find! (zodiac:begin0-form-rest ast))]
		     
		     ;;--------------------------------------------------------
		     ;; SET! EXPRESSIONS
		     ;;
		     ;; we analyze the target, which will register it as being
		     ;; mutable or used, as necessary.  Then we analyze the value.
		     ;;
		     [(zodiac:set!-form? ast)
		      (find! (zodiac:set!-form-val ast))]
		     
		     ;;---------------------------------------------------------
		     ;; DEFINE EXPRESSIONS
		     ;;
		     [(zodiac:define-values-form? ast)
		      (find! (zodiac:define-values-form-val ast))]
		     
		     ;;---------------------------------------------------------
		     ;; DEFINE-SYNTAXES
		     ;;
		     [(zodiac:define-syntaxes-form? ast)
		      (find! (zodiac:define-syntaxes-form-expr ast))]
		     
		     ;;-------------------------------------------------------------------
		     ;; APPLICATIONS
		     ;;  analyze all the parts, and note whether the rator is
		     ;;  a primitive;
		     ;;  if this is a call to a primitive, check the arity.
		     ;;
		     [(zodiac:app? ast)
		      (find! (zodiac:app-fun ast))
		      (for-each find! (zodiac:app-args ast))]
		     
		     ;;-------------------------------------------------------------------
		     ;; WITH-CONTINUATION-MARK
		     ;;
		     ;; analyze the key, val, and body
		     ;;
		     [(zodiac:with-continuation-mark-form? ast)
		      (find! (zodiac:with-continuation-mark-form-key ast))
		      (find! (zodiac:with-continuation-mark-form-val ast))
		      (find! (zodiac:with-continuation-mark-form-body ast))]

		     ;;-----------------------------------------------------------
		     ;; GLOBALS
		     ;;
		     [(zodiac:global-prepare? ast)
		      (find! (zodiac:global-prepare-vec ast))]
		     [(zodiac:global-lookup? ast)
		      (find! (zodiac:global-lookup-vec ast))]
		     [(zodiac:global-assign? ast)
		      (find! (zodiac:global-assign-vec ast))
		      (find! (zodiac:global-assign-expr ast))]
		     [(zodiac:safe-vector-ref? ast)
		      (find! (zodiac:safe-vector-ref-vec ast))]
		     
		     [else (compiler:internal-error
			    ast
			    (format "unsupported syntactic form (~a)"
				    (if (struct? ast)
					(vector-ref (struct->vector ast) 0)
					ast)))]))])
	  
	  (lambda (ast)
	    (set! procedures null)
	    (find! ast))))

      ;; Recursively determines the `liftable' field in the procedure
      ;; record. If a cycle is encountered, return 'cycle or 'pls-cycle
      ;; (the latter of any part of the cycle had "globals" in its
      ;; "closure")
      (define (get-liftable! lambda)
	(let ([code (get-annotation lambda)])
	  (if (eq? (procedure-code-liftable code) 'unknown-liftable)
	      (if (and lifting-allowed?
		       (or mutual-lifting-allowed? 
			   (set-empty? (code-free-vars code)))
		       (or single-module-mode?
			   (andmap (case-lambda 
				    [(var)
				     (or (not (mod-glob? var))
					 (let ([modname (mod-glob-modname var)])
					   (if (eq? modname '#%kernel)
					       #t
					       (not modname))))])
				   (set->list (code-global-vars code)))))
		  ;; Liftable only if there are no free (non-pls) global vars
		  (begin
		    ;; Mark this one in case we encounter a cycle:
		    (set-procedure-code-liftable! code 'cycle)
		    ;; Check each free variable
		    (let ([r (let loop ([l (set->list (code-free-vars code))]
					[pls? (or per-load-lifting-only?
						  (not (set-empty? (code-global-vars code))))]
					[cycle? #f])
			       (if (null? l)
				   ;; It's liftable, assuming any cycles are resolved
				   (cond
				    [(and pls? cycle?) 'pls-cycle]
				    [cycle? 'cycle]
				    [pls? 'pls]
				    [else 'static])
				   
				   ;; Check the free variable - references a liftable proc?
				   (let ([v (extract-ast-known-value (extract-varref-known-val (car l)))])
				     (if (zodiac:case-lambda-form? v)
					 (let ([vl (let ([l (get-liftable! v)])
						     (cond
						      [(top-level-varref/bind-from-lift? l)
						       ;; lifted in a previous phase
						       (if (top-level-varref/bind-from-lift-pls? l)
							   'pls
							   'static)]
						      [(pair? l)
						       ;; lifted already in this phase
						       (if (top-level-varref/bind-from-lift-pls? (car l))
							   'pls
							   'static)]
						      [else l]))])
					   (if vl
					       (loop (cdr l)
						     (or pls? (eq? vl 'pls) (eq? vl 'pls-cycle))
						     (or cycle? (eq? vl 'cycle) (eq? vl 'pls-cycle)))
					       #f))
					 #f))))])
		      (cond
		       [(not r) (set-procedure-code-liftable! code #f) 
			#f]
		       [(or (eq? r 'cycle) (eq? r 'pls-cycle))
			(set-procedure-code-liftable! code 'unknown-liftable) 
			r]
		       [(eq? r 'pls) (set-procedure-code-liftable! code 'pls) 
			'pls]
		       [else (set-procedure-code-liftable! code 'static) 
			     'static])))
		  (begin
		    (set-procedure-code-liftable! code #f)
		    #f))
	      (procedure-code-liftable code))))

      (define (set-liftable! lambda)
	(unless (top-level-varref/bind-from-lift? (procedure-code-liftable (get-annotation lambda)))
	  (let ([v (get-liftable! lambda)])
	    (when v
	      (let ([pls? (or (eq? v 'pls) (eq? v 'pls-cycle))])
		(when (compiler:option:verbose)
		  (compiler:warning lambda (format "found static procedure~a"
						   (if pls? " (per-load)" ""))))
		(compiler:add-lifted-lambda! lambda pls?))))))

      ;; lift-lambdas! uses the `liftable' procedure annotation with known-value
      ;;  analysis to replace lexical variables referencing known liftable
      ;;  procedures with top-level-varrefs referencing the lifted
      ;;  procedure
      ;; Since per-load lifting rarranges global variable sets, we
      ;;  recompute them.
      ;; Returns (cons ast global-var-set)
      (define lift-lambdas!
	(letrec ([lift!
		  (lambda (ast code)
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
		     [(zodiac:bound-varref? ast)
		      (let ([v (extract-ast-known-value (extract-varref-known-val ast))])
						
			(if (zodiac:case-lambda-form? v)

			    (let ([lifted (let ([l (procedure-code-liftable (get-annotation v))])
					    (if (pair? l) (car l) l))])
			      (if lifted
				  
				  ;; The procedure was lifted
				  (begin
				    (when code
				      (remove-code-free-vars! code (make-singleton-set 
								    (zodiac:bound-varref-binding ast))))
				    (when (top-level-varref/bind-from-lift-pls? lifted)
				      (add-global! const:the-per-load-statics-table))
				    lifted)
				  
				  ;; No change
				  ast))

			    ;; No change
			    ast))]
		     
		     [(zodiac:top-level-varref? ast)

		      (cond
		       [(varref:has-attribute? ast varref:primitive)
			(void)]
		       [(varref:has-attribute? ast varref:per-load-static)
			(add-global! const:the-per-load-statics-table)]
		       [(varref:has-attribute? ast varref:static)
			(void)]
		       [else (add-global! (compiler:add-global-varref! ast))])
		      
		      ast]
		     
		     ;;--------------------------------------------------------------------
		     ;; LAMBDA EXPRESSIONS
		     ;;    analyze the bodies, and set binding info for the binding vars
		     ;;
		     [(zodiac:case-lambda-form? ast)
		      (let ([code (get-annotation ast)]
			    [save-globals globals])
			;; We're recomputing globals...
			(set-code-global-vars! code empty-set)

			(zodiac:set-case-lambda-form-bodies! 
			 ast 
			 (map (lambda (b ccode)
				(set! globals empty-set)
				;; Analyze case
				(let ([b (lift! b ccode)])
				  ;; Set and merge globals
				  (set-code-global-vars! ccode globals)
				  (set-code-global-vars! code
							 (set-union globals
								    (code-global-vars code)))

				  b))
			      (zodiac:case-lambda-form-bodies ast)
			      (procedure-code-case-codes code)))
			
			;; If it being lifted ON THIS PASS, the value of `lifted' will be a list;
			;;  in that case, return the new static varref
			(let ([lifted (procedure-code-liftable code)])
			  (if (or (not lifted) (top-level-varref/bind-from-lift? lifted))
			      
			      ;; Not lifted (or not on this pass)
			      (begin
				(set! globals (set-union save-globals (code-global-vars code)))
				ast)
			      
			      ;; Lifting on this pass
			      (let ([lifted (car lifted)])
				(set-procedure-code-liftable! code lifted)
				
				(if (top-level-varref/bind-from-lift-pls? lifted)
				    (set! globals (set-union-singleton 
						   save-globals
						   const:the-per-load-statics-table))
				    (set! globals save-globals))
				
				lifted))))]

		     ;;--------------------------------------------------------------
		     ;; LET EXPRESSIONS
		     ;;
		     [(zodiac:let-values-form? ast)
		      (let* ([val (lift! (car (zodiac:let-values-form-vals ast)) code)])
			(set-car! (zodiac:let-values-form-vals ast) val)
			
			;; lift in body expressions
			(let ([body (lift! (zodiac:let-values-form-body ast) code)])

			  (if (and (= 1 (length (car (zodiac:let-values-form-vars ast))))
				   (top-level-varref/bind-from-lift? val)
				   (not (binding-mutable? (get-annotation (caar (zodiac:let-values-form-vars ast))))))

			      ;; Let binding value is a lifted procedure, drop the variable
			      (let ([var (caar (zodiac:let-values-form-vars ast))])
				(remove-local-var! code var)
				body)

			      (begin
				(zodiac:set-let-values-form-body! ast body)
				ast))))]

		     ;;-----------------------------------------------------------------
		     ;; LETREC EXPRESSIONS
		     ;;
		     [(zodiac:letrec-values-form? ast)
		      
		      (let* ([varses (zodiac:letrec-values-form-vars ast)]
			     [vals (zodiac:letrec-values-form-vals ast)])
			
			(zodiac:set-letrec-values-form-vals! 
			 ast 
			 (map (lambda (val) (lift! val code)) vals))
			
			(zodiac:set-letrec-values-form-body!
			 ast
			 (lift! (zodiac:letrec-values-form-body ast) code))
			
			(let loop ([varses varses][vals (zodiac:letrec-values-form-vals ast)]
				   [vss-accum null][vs-accum null])
			  (if (null? varses)

			      (begin
				(zodiac:set-letrec-values-form-vars! ast (reverse! vss-accum))
				(zodiac:set-letrec-values-form-vals! ast (reverse! vs-accum)))

			      (let ([vars (car varses)]
				    [val (car vals)])
				(if (and (= 1 (length vars))
					 (top-level-varref/bind-from-lift? val))
				    ;; Let binding value is a lifted procedure, drop the variable
				    (begin
				      (remove-local-var! code (car vars))
				      (loop (cdr varses) (cdr vals) vss-accum vs-accum))

				    ;; Normal binding
				    (loop (cdr varses) (cdr vals) (cons vars vss-accum) (cons val vs-accum))))))

			(if (null? (zodiac:letrec-values-form-vars ast))

			    ;; All binding values were lifted; return the body
			    (zodiac:letrec-values-form-body ast)
			    
			    ast))]
		     
		     ;;-----------------------------------------------------
		     ;; IF EXPRESSIONS
		     ;;
		     ;;  analyze the 3 branches.
		     ;;
		     [(zodiac:if-form? ast)
		      (zodiac:set-if-form-test! ast (lift! (zodiac:if-form-test ast) code))
		      (let ([then (lift! (zodiac:if-form-then ast) code)]
			    [else (lift! (zodiac:if-form-else ast) code)])
			(zodiac:set-if-form-then! ast then)
			(zodiac:set-if-form-else! ast else)
			
			ast)]
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN EXPRESSIONS
		     ;;
		     [(zodiac:begin-form? ast)
		      
		      (zodiac:set-begin-form-bodies!
		       ast
		       (map (lambda (b) (lift! b code))
			    (zodiac:begin-form-bodies ast)))
		      
		      ast]
		     
		     
		     ;;--------------------------------------------------------
		     ;; BEGIN0 EXPRESSIONS
		     ;;
		     ;; analyze the branches
		     [(zodiac:begin0-form? ast)
		      (zodiac:set-begin0-form-first! ast (lift! (zodiac:begin0-form-first ast) code))
		      (zodiac:set-begin0-form-rest! ast (lift! (zodiac:begin0-form-rest ast) code))

		      ast]
		     
		     ;;--------------------------------------------------------
		     ;; SET! EXPRESSIONS
		     ;;
		     ;;
		     [(zodiac:set!-form? ast)

		      ;; Possibly a top-level-varref; put it in the global-var set
		      (lift! (zodiac:set!-form-var ast) code)

		      (zodiac:set-set!-form-val! 
		       ast 
		       (lift! (zodiac:set!-form-val ast) code))
		      
		      ast]
		     
		     ;;---------------------------------------------------------
		     ;; DEFINE EXPRESSIONS
		     ;;
		     [(zodiac:define-values-form? ast)
		      
		      ;; Top-level-varrefs; put them in the global-var set
		      (for-each (lambda (v) (lift! v code)) (zodiac:define-values-form-vars ast))

		      (zodiac:set-define-values-form-val! 
		       ast
		       (lift! (zodiac:define-values-form-val ast) code))
		      
		      ast]
		     
		     ;;---------------------------------------------------------
		     ;; DEFINE-SYNTAXES
		     ;;
		     [(zodiac:define-syntaxes-form? ast)
		      
		      (zodiac:set-define-syntaxes-form-expr!
		       ast
		       (lift! (zodiac:define-syntaxes-form-expr ast) code))
		      
		      ast]
		     
		     ;;-------------------------------------------------------------------
		     ;; APPLICATIONS
		     ;;  analyze all the parts, and note whether the rator is
		     ;;  a primitive;
		     ;;  if this is a call to a primitive, check the arity.
		     ;;
		     [(zodiac:app? ast)
		      
		      (let* ([fun (lift! (zodiac:app-fun ast) code)]
			     [args (map (lambda (arg) (lift! arg code))
					(zodiac:app-args ast))])
			(zodiac:set-app-fun! ast fun)
			(zodiac:set-app-args! ast args))

		      ast]
		     
		     ;;-------------------------------------------------------------------
		     ;; WITH-CONTINUATION-MARK
		     ;;
		     ;; analyze the key, val, and body
		     ;;
		     [(zodiac:with-continuation-mark-form? ast)

		      (zodiac:set-with-continuation-mark-form-key!
		       ast
		       (lift! (zodiac:with-continuation-mark-form-key ast) code))
		      
		      (zodiac:set-with-continuation-mark-form-val!
		       ast
		       (lift! (zodiac:with-continuation-mark-form-val ast) code))
		      
		      (zodiac:set-with-continuation-mark-form-body!
		       ast
		       (lift! (zodiac:with-continuation-mark-form-body ast) code))

		      ast]

		     ;;-----------------------------------------------------------
		     ;; GLOBALS
		     ;;
		     [(zodiac:global-prepare? ast)
		      (zodiac:set-global-prepare-vec!
		       ast
		       (lift! (zodiac:global-prepare-vec ast) code))
		      ast]
		     [(zodiac:global-lookup? ast)
		      (zodiac:set-global-lookup-vec!
		       ast
		       (lift! (zodiac:global-lookup-vec ast) code))
		      ast]
		     [(zodiac:global-assign? ast)
		      (zodiac:set-global-assign-vec!
		       ast
		       (lift! (zodiac:global-assign-vec ast) code))
		      (zodiac:set-global-assign-expr!
		       ast
		       (lift! (zodiac:global-assign-expr ast) code))
		      ast]
		     [(zodiac:safe-vector-ref? ast)
		      (zodiac:set-safe-vector-ref-vec!
		       ast
		       (lift! (zodiac:safe-vector-ref-vec ast) code))
		      ast]

		     [else (compiler:internal-error
			    ast
			    (format "unsupported syntactic form (~a)"
				    (if (struct? ast)
					(vector-ref (struct->vector ast) 0)
					ast)))]))]
		 [remove-local-var! (lambda (code var)
				      (let ([vars (make-singleton-set var)])
					(set-code-local-vars! code (set-minus (code-local-vars code) vars))
					(set-code-captured-vars! code (set-minus (code-captured-vars code) vars))
					;; Is it a case code?
					(when (case-code? code)
					  (let ([code (code-parent code)])
					    (remove-local-var! code var)))))]
		 [globals empty-set]
		 [add-global! (lambda (v) (set! globals (set-union-singleton globals v)))])
	  
	  (lambda (ast code)
	    ;; Find all the procedures
	    (find-all-procedures! ast)

	    ;; If we marked it as unliftable before, mark it as unknown
	    ;;  now because we'll check again:
	    (for-each
	     (lambda (l)
	       (let ([l (car l)])
		 (let ([c (get-annotation l)])
		   (unless (procedure-code-liftable c)
		     (set-procedure-code-liftable! c 'unknown-liftable)))))
	     procedures)

	    ;; Set liftable flags
	    (for-each (lambda (l)
			(let ([l (car l)])
			  (set-liftable! l)))
		      procedures)

	    (set! globals empty-set)
	    (let ([ast (lift! ast code)])
	      (cons ast globals)))))))
