;; Scheme->VMScheme conversion phase
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT, Rice University

;; Takes a zodiac:* AST and produces a vm:* AST. Some
;;  zodiac:* AST elements are still used, particularly
;;  bindings and varrefs. 

;; Well-applied known primitives are sometimes compiled
;;  to macro uses (where the macros are defined in mzc.h).

(module vmphase mzscheme 
  (require (lib "unit.ss")
	   (lib "list.ss")
	   (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax")
	   (lib "primitives.ss" "syntax"))

  (require "sig.ss"
	   "../sig.ss")

  (provide vmphase@)
  (define-unit vmphase@
      (import (prefix compiler:option: compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (prefix zodiac: zodiac^)
	      compiler:zlayer^
	      compiler:analyze^
	      compiler:const^
	      compiler:vmstructs^
	      compiler:rep^
	      compiler:closure^
	      compiler:vehicle^
	      compiler:driver^)
      (export compiler:vmphase^)

      ;; vm:convert-bound-varref takes a bound-varref and turns it 
      ;; into a vm:local-varref, taking into account its representation.
      (define vm:convert-bound-varref
	(lambda (ast)
	  (let* ([ref (make-vm:local-varref
		       (zodiac:zodiac-stx ast)
		       (zodiac:varref-var ast)
		       (zodiac:bound-varref-binding ast))]
		 
		 ;; this might not be true in the future, but it is true now
		 [boxed? (rep:pointer?
			  (binding-rep
			   (compiler:bound-varref->binding ast)))]
		 [ref (if boxed?
			  (make-vm:deref #f ref)
			  ref)])
	    ref)))

      (define (check-primitive-as-macro prim argc prim-k normal-k)
	(if (and prim 
		 (or (eq? prim 'for-syntax-in-env)
		     (procedure-arity-includes? (dynamic-require 'mzscheme prim) argc)))
	    (let* ([argc=? (lambda (x) (= x argc))]
		   [special-bool (case prim
				   [(eq?) "MZC_EQP"]
				   [(eqv?) "MZC_EQVP"]
				   [(equal?) "MZC_EQUALP"]
				   [(null?) "MZC_NULLP"]
				   [(pair?) "MZC_PAIRP"]
				   [(not) "MZC_NOTP"]
				   [(symbol?) "MZC_SYMBOLP"]
				   [(string?) "MZC_STRINGP"]
				   [(bytes?) "MZC_BYTESP"]
				   [(vector?) "MZC_VECTORP"]
				   [(number?) "MZC_NUMBERP"]
				   [(procedure?) "MZC_PROCEDUREP"]
				   [(char?) "MZC_CHARP"]
				   [(eof-object?) "MZC_EOFP"]
				   [(zero?) "MZC_ZEROP"]
				   [(<) (and (argc=? 2) "MZC_LTP")]
				   [(>) (and (argc=? 2) "MZC_GTP")]
				   [(<=) (and (argc=? 2) "MZC_LTEP")]
				   [(>=) (and (argc=? 2) "MZC_GTEP")]
				   [(=) (and (argc=? 2) "MZC_EQLP")]
				   [else #f])])
	      (if special-bool
		  (prim-k special-bool #t)
		  (let ([special (case prim
				   [(cons) "MZC_CONS"]
				   [(list) (cond
					      [(argc=? 1) "MZC_LIST1"]
					      [(argc=? 2) "MZC_LIST2"]
					      [else #f])]
				   [(append) (and (argc=? 2) "MZC_APPEND")]
				   [(car) "MZC_CAR"]
				   [(cdr) "MZC_CDR"]
				   [(cadr) "MZC_CADR"]
				   [(cddr) "MZC_CDDR"]
				   [(caar) "MZC_CAAR"]
				   [(cdar) "MZC_CDAR"]
				   [(caddr) "MZC_CADDR"]
				   [(set-car!) "MZC_SET_CAR"]
				   [(set-cdr!) "MZC_SET_CDR"]
				   [(vector-ref) "MZC_VECTOR_REF"]
				   [(vector-set!) "MZC_VECTOR_SET"]
				   [(string-ref) "MZC_STRING_REF"]
				   [(string-set!) "MZC_STRING_SET"]
				   [(bytes-ref) "MZC_BYTES_REF"]
				   [(bytes-set!) "MZC_BYTES_SET"]
				   [(char->integer) "MZC_CHAR_TO_INTEGER"]
				   [(add1) "MZC_ADD1"]
				   [(sub1) "MZC_SUB1"]
				   [(+) (and (argc=? 2) "MZC_PLUS2")]
				   [(-) (and (argc=? 2) "MZC_MINUS2")]
				   [(*) (and (argc=? 2) (compiler:option:fixnum-arithmetic) "MZC_TIMES2")]
				   [(min) (and (argc=? 2) "MZC_MIN2")]
				   [(max) (and (argc=? 2) "MZC_MAX2")]
				   [(quotient) (and (argc=? 2) "MZC_QUOTIENT")]
				   [(for-syntax-in-env) "MZC_FOR_SYNTAX_IN_ENV"]
				   [else #f])])
		    (if special
			(prim-k special #f)
			(normal-k)))))
	    (normal-k)))


      (define (simple-tail-prim? prim)
	;; Since "simple" primitives don't end with a tail call,
	;;  there's no harm in calling them directly when
	;;  they're in a tail position. We avoid he overhead of
	;;  a tail call this way.
	(and prim (not (memq prim (internal-tail-chain-prims)))))

      ;; vm-phase takes 2 arguments:
      ;; 1) an s-expression to be transformed
      ;; 2) a value which may be #f for non-tail transformation,
      ;;    or a procedure to apply to a value found in tail position
      ;; 3) a boolean value for whether this expression is in tail
      ;;    position or not.
      ;;
      ;; and returns 2 values:
      ;; 1) a vm-scheme sequence
      ;; 2) new local variables introduced

      (define (vm-phase ast multi? leaf tail-pos tail? magic?)
	(letrec 
	    ([new-locals empty-set]
	     [add-new-local! (lambda (l)
			       (set! new-locals
				     (set-union (list->set (list l)) new-locals)))]
	     [make-record (lambda (pointer env-rep)
			    (make-vm:set! #f (list (cons target-type:lexical pointer))
					  (make-vm:alloc #f env-rep) #f))]
	     [make-closure (lambda (leaf tail-pos tail? pointer vehicle L name closure-rep)
			     (let ([m (cond
				       [(zodiac:case-lambda-form? L)
					(if (= 1 (length (zodiac:case-lambda-form-args L)))
					    (let-values ([(min-arity max-arity)
							  (compiler:formals->arity
							   (car (zodiac:case-lambda-form-args L)))])
					      (make-vm:make-procedure-closure #f
									      pointer
									      vehicle
									      min-arity
									      max-arity
									      name
									      (not closure-rep)
									      (procedure-code-method? (get-annotation L))))
					    (make-vm:make-case-procedure-closure #f
										 pointer
										 vehicle
										 (length (zodiac:case-lambda-form-args L))
										 (procedure-code-case-arities (get-annotation L))
										 name
										 (not closure-rep)
										 (procedure-code-method? (get-annotation L))))]
				       [else
					(compiler:internal-error
					 #f
					 "unknown closure kind: ~a" L)])])
			       (if tail-pos (leaf (tail-pos m)) (leaf m))))]
	     
	     [fill-label (lambda (pointer code)
			   (if (vehicle:only-code-in-vehicle? code)
			       null ; no label field in this case
			       (list (make-vm:set!
				      #f
				      (list 
				       (cons
					target-type:lexical
					(make-vm:struct-ref #f 'label 
							    (make-vm:struct-ref #f 'data
										(make-vm:deref #f 
											       pointer)))))
				      (make-vm:immediate #f (closure-code-label code))
				      #f))))]

	     [fill-env
	      (lambda (pointer code)
		(map (lambda (field var)
		       (if (zodiac:binding? var)
			   ;; Local variable
			   (let* ([var (convert
					(zodiac:binding->lexical-varref var)
					#f
					identity
					#f
					#f
					#t)]
				  ;; we have to copy pointers if necessary!
				  [var (if (vm:deref? var)
					   (vm:deref-var var)
					   var)])
			     (make-vm:set! #f 
					   (list (cons
						  target-type:lexical
						  (make-vm:struct-ref #f 
								      (rep:struct-field-name field)
								      (make-vm:struct-ref #f 'data
											  (make-vm:deref #f 
													 pointer)))))
					   var #f))
			   ;; Propogate global bucket
			   (let* ([var (cond
					[(const:per-load-statics-table? 
					  (rep:struct-field-orig-name field))
					 (make-vm:per-load-statics-table #f)]
					[else
					 (make-vm:bucket #f var)])])
			     (make-vm:set! #f 
					   (list (cons
						  target-type:lexical
						  (make-vm:struct-ref #f 
								      (rep:struct-field-name field)
								      (make-vm:struct-ref #f 'data
											  (make-vm:deref #f 
													 pointer)))))
					   var #f))))
		     (let ([fields (let ([cr (closure-code-rep code)])
				     (if cr 
					 (rep:struct-fields cr)
					 null))])
		       (if (vehicle:only-code-in-vehicle? code)
			   fields
			   (cdr fields))) ; knock the label field off
		     (append (set->list (code-free-vars code))
			     (set->list (code-global-vars code)))))]
	     [convert
	      (lambda (ast multi? leaf tail-pos tail? used?)
		(when (compiler:option:debug)
		  (zodiac:print-start! (debug:get-port) ast)
		  (fprintf (debug:get-port) "~a~n" ast))
		(cond
		 
		 ;;-----------------------------------------------------------------
		 ;; BEGIN FORM
		 ;;
		 [(zodiac:begin-form? ast)
		  (apply append!
			 (begin-map!
			  ;; non-tail, not used
			  (lambda (b) (convert b 
					       #t
					       list 
					       (lambda (x) (make-vm:void #f x #f))
					       #f
					       #f))
			  ;; tail
			  (lambda (b) (convert b multi? leaf tail-pos tail? used?))
			  ;; list
			  (zodiac:begin-form-bodies ast)))]
		 
		 ;;-----------------------------------------------------------------
		 ;; BEGIN0 FORM
		 ;;
		 [(zodiac:begin0-form? ast)
		  (let* ([var (convert (zodiac:binding->lexical-varref
					(get-annotation ast))
				       #f
				       identity
				       #f
				       #f
				       #t)]
			 [first (convert (zodiac:begin0-form-first ast) 
					 multi?
					 (lambda (val) (list (make-vm:begin0-mark! #f var val)))
					 #f #f #t)]
			 [rest (convert (zodiac:begin0-form-rest ast) #t list #f #f #f)]
			 [begin0-setup
			  (make-vm:begin0-setup!
			   (zodiac:zodiac-stx ast)
			   var)]
			 [begin0-extract
			  (make-vm:begin0-extract
			   (zodiac:zodiac-stx ast)
			   var)])
		    (append first
			    (list begin0-setup)
			    rest
			    (if tail-pos
				(leaf (tail-pos begin0-extract))
				(leaf begin0-extract))))]

		 ;;-----------------------------------------------------------------
		 ;; IF FORM
		 ;;
		 [(zodiac:if-form? ast)
		  (let ([test (convert (zodiac:if-form-test ast) #f list #f #f #t)]
			[then (convert (zodiac:if-form-then ast) multi? leaf tail-pos tail? used?)]
			[else (convert (zodiac:if-form-else ast) multi? leaf tail-pos tail? used?)])
		    (list (make-vm:if     
			   (zodiac:zodiac-stx ast)
			   test
			   (make-vm:sequence #f
					     then)
			   (make-vm:sequence #f
					     else))))]
		 
		 ;;-----------------------------------------------------------------
		 ;; LET FORM
		 ;;
		 ;; let forms are more complicated, since we have to reduce their
		 ;; strength.  We send along a leaf function to set! the variable
		 ;; to the result of the let.
		 ;; if we are binding a variable that lives at the end of a pointer
		 ;; make that box AFTER the calculation -- temporary variable needed
		 ;; this is to avoid exposing box creation to call/cc
		 ;; note that mv-application automatically creates a temporary
		 ;; container
		 ;;
		 ;; let [x A] M --> (set! x A) M ...
		 ;; let [x A] M, where A is mutable --> (set! t A) (set! x box) (set-box! x t)
		 ;; let [x (A A*)] M --> {set up args, set! apply} M ...
		 ;; let [x (if A M M)] M --> (if A M[t/set!] M[t/set!]) M ...
		 ;; let [x (lambda)] M --> {make-closure,set!} M ...
		 ;;
		 [(zodiac:let-values-form? ast)
		  (let* ([vars (map (lambda (vref)
				      (cons
				       target-type:lexical
				       (convert (zodiac:binding->lexical-varref vref)
						#f
						identity
						#f
						#f
						#t)))
				    (car (zodiac:let-values-form-vars ast)))]
			 [val (car (zodiac:let-values-form-vals ast))]
			 [reps (map (lambda (bound)
				      (binding-rep
				       (get-annotation bound)))
				    (car (zodiac:let-values-form-vars ast)))]
			 [temps-needed? (ormap (lambda (zb)
						 (let ([b (get-annotation zb)])
						   (or (binding-mutable? b)
						       (binding-letrec-set? b)
						       (binding-letrec-set? b))))
					       (car (zodiac:let-values-form-vars ast)))]
			 [body (convert (zodiac:let-values-form-body ast) multi? leaf tail-pos tail? used?)])
		    
		    (if (not temps-needed?)
			
			(append! (convert val
					  (not (= 1 (length vars)))
					  (lambda (val) (list (make-vm:set! #f vars val #f)))
					  #f
					  #f
					  #t)
				 body)
			
			(let* ([tnames (map (lambda (_) (compiler:gensym)) vars)]
			       [tbounds (map (lambda (name rep)
					       (let ([b (zodiac:make-binding 
							 #f
							 (make-empty-box)
							 name name)])
						 (set-annotation! b
								  (make-binding #f #t #f #f #f #f #f #f #f
										(if (rep:pointer? rep)
										    (rep:pointer-to rep)
										    rep)))
						 b))
					     tnames
					     reps)]			 
			       [trefs (map (lambda (bound)
					     (convert (zodiac:binding->lexical-varref bound) 
						      #f
						      identity
						      #f
						      #f
						      #t))
					   tbounds)]
			       [set-temps
				(convert val
					 (not (= 1 (length vars)))
					 (lambda (val)
					   (list (make-vm:set! 
						  #f
						  (map (lambda (r) (cons target-type:lexical r)) trefs)
						  val #f)))
					 #f #f #t)]
			       [make-boxes 
				(apply append!
				       (map (lambda (rep var)
					      (if (rep:pointer? rep)
						  (list 
						   (make-vm:set! 
						    #f
						    (list (cons target-type:lexical
								(vm:deref-var (cdr var))))
						    (make-vm:alloc #f (rep:pointer-to rep))
						    #f))
						  null))
					    reps
					    vars))]
			       
			       [set-vars
				(map (lambda (var tvar) (make-vm:set! #f (list var) tvar #f))
				     vars
				     trefs)])
			  (for-each add-new-local! tbounds)
			  (append! set-temps make-boxes set-vars body)) 		  
			))]
		 
		 ;;-----------------------------------------------------------------
		 ;; LETREC FORM
		 ;;
		 ;; allocate all closures first, then create closure objects, then
		 ;; fill in the structs
		 ;;
		 ;; inline closure allocation (use closure-alloc-rep)
		 ;; Handle non-lambdas because thunk-allocation optimization may
		 ;; have taken place
		 ;;
		 [(zodiac:letrec-values-form? ast)
		  (let* ([Ls (foldr (lambda (val l)
				      (if (compiler:make-closure? val)
					  (cons (compiler:make-closure-lambda val)
						l)
					  l))
				    null
				    (zodiac:letrec-values-form-vals ast))]
			 [codes (map get-annotation Ls)]
			 [closure-reps (map closure-code-rep codes)]
			 [closure-alloc-reps (map closure-code-alloc-rep codes)]
			 [vehicles (map closure-code-vehicle codes)]
			 [new-bounds
			  (map (lambda (closure-alloc-rep)
				 (let* ([n (compiler:gensym)]
					[b (zodiac:make-binding 
					    #f
					    (make-empty-box)
					    n
					    n)])
				   (set-annotation! b
						    (make-binding #f ;rec?
								  #t ;mutable?
								  #f ;unit-i/e?
								  #f ;anchor
								  #f ;letrec-set?
								  #f ;ivar?
								  #f ;known?
								  #f ;val
								  #f ;known-but-used?
								  (make-rep:pointer closure-alloc-rep)))
				   b))
			       closure-alloc-reps)]
			 [new-vars (map
				    (lambda (b)
				      (convert (zodiac:binding->lexical-varref b)
					       #f
					       identity
					       #f
					       #f
					       #t))
				    new-bounds)]
			 [pointers (map vm:deref-var new-vars)]
			 [vars (foldr (lambda (var val l)
					(if (compiler:make-closure? val)
					    (cons (convert (zodiac:binding->lexical-varref 
							    (car var))
							   #f
							   identity
							   #f
							   #f
							   #t)
						  l)
					    l))
				      null
				      (zodiac:letrec-values-form-vars ast)
				      (zodiac:letrec-values-form-vals ast))]
			 [names (foldr (lambda (var val l)
					 (if (compiler:make-closure? val)
					     (cons (compiler:make-closure-name val)
						   l)
					     l))
				       null
				       (zodiac:letrec-values-form-vars ast)
				       (zodiac:letrec-values-form-vals ast))]
			 [nonrec-assigns 
			  (foldr (lambda (var val l)
				   (if (compiler:make-closure? val)
				       l
				       (cons (make-vm:set! 
					      #f
					      (list (cons
						     target-type:lexical
						     (convert (zodiac:binding->lexical-varref 
							       (car var))
							      #f
							      identity
							      #f
							      #f
							      #t)))
					      (convert val
						       #f
						       identity
						       #f
						       #f
						       #t)
					      #f)
					     l)))
				 null
				 (zodiac:letrec-values-form-vars ast)
				 (zodiac:letrec-values-form-vals ast))])
		    (for-each add-new-local! new-bounds)
		    (append!
		     nonrec-assigns
		     (map make-record pointers closure-alloc-reps)
		     (map (lambda (var pointer vehicle L name closure-rep)
			    (make-closure (lambda (c) (make-vm:set! #f
								    (list (cons
									   target-type:lexical
									   var))
								    c #f))
					  #f
					  #f
					  pointer
					  vehicle
					  L
					  name
					  closure-rep))
			  vars
			  pointers
			  vehicles
			  Ls
			  names
			  closure-reps)
		     (apply append (map fill-label pointers codes))
		     (apply append! (map fill-env pointers codes))
		     (convert (zodiac:letrec-values-form-body ast) multi? leaf tail-pos tail? used?)))]
		 
		 
		 
		 ;;-----------------------------------------------------------------
		 ;; MAKE-CLOSURE FORM
		 ;;
		 ;; we make a struct, create the closure, then fill it in
		 ;;
		 ;; inline closure allocation (use closure-alloc-rep)
		 [(compiler:make-closure? ast)
		  (let* ([L (compiler:make-closure-lambda ast)]
			 [name (compiler:make-closure-name ast)]
			 [code (get-annotation L)]
			 [label (closure-code-label code)]
			 [closure-rep (closure-code-rep code)]
			 [closure-alloc-rep (closure-code-alloc-rep code)]
			 [vehicle (closure-code-vehicle code)]
			 [n (compiler:gensym)]
			 ;; a variable in which to construct the closure
			 [new-bound 
			  (if closure-alloc-rep
			      (zodiac:make-binding 
			       (zodiac:zodiac-stx L)
			       (make-empty-box)
			       n
			       n)
			      #f)]
			 [_ (when new-bound
			      (set-annotation! new-bound
					       (make-binding #f ;rec?
							     #t ;mutable?
							     #f ;unit-i/e?
							     #f ;anchor
							     #f ;letrec-set?
							     #f ;ivar?
							     #f ;known?
							     #f ;val
							     #f ;known-but-used?
							     (make-rep:pointer closure-alloc-rep))))]
			 ;; the reference to the closure
			 [new-var (and new-bound
				       (convert (zodiac:binding->lexical-varref new-bound)
						#f
						identity
						#f
						#f
						#t))]
			 ;; the reference to the pointer to the closure
			 [pointer (and new-var (vm:deref-var new-var))]
			 
			 ;; set up arguments to closure-maker
			 [make-args (map (lambda (a) (convert a #f identity #f #f #t)) (compiler:make-closure-args ast))]
			 [get-args (if (null? make-args)
				       ()
				       (list (make-vm:generic-args (zodiac:zodiac-stx ast)
								   #f #f #f #f make-args)))])
		    (set-closure-code-label! code label)
		    (when new-bound
		      (add-new-local! new-bound))
		    `( ,@get-args
		       ,@(if closure-alloc-rep
			     (list (make-record pointer closure-alloc-rep))
			     null)
		       ,@(fill-label pointer code)
		       ,@(fill-env pointer code)
		       ,@(make-closure leaf 
				       tail-pos 
				       tail? 
				       pointer
				       vehicle
				       L
				       name
				       closure-rep)))]
		 
		 ;;-----------------------------------------------------------------
		 ;; SET! FORM
		 ;;
		 ;; we need to distinguish between setting a global & reffing it;
		 ;; if we are in tail position, we need to do the void thing
		 ;;
		 [(zodiac:set!-form? ast)
		  (let* ([var (zodiac:set!-form-var ast)]
			 [val (convert (zodiac:set!-form-val ast)
				       #f
				       identity #f #f #t)]
			 [set!-exp
			  (make-vm:set!		     
			   (zodiac:zodiac-stx ast)
			   (list (if (zodiac:top-level-varref? var)
				     (cons target-type:global (compiler:add-global-varref! var))
				     (cons target-type:lexical 
					   (convert var
						    #f
						    identity
						    #f
						    #f
						    #t))))
			   val
			   (if (zodiac:top-level-varref? var)
			       (list "set!" 0)
			       #f))])
		    (if tail-pos
			(cons set!-exp
			      (leaf (tail-pos (make-vm:immediate #f
								 (zodiac:make-special-constant 'void)))))
			(if used?
			    (cons set!-exp
				  (leaf (make-vm:immediate #f
							   (zodiac:make-special-constant 'void))))
			    (leaf set!-exp))))]
		 
		 ;;-----------------------------------------------------------------
		 ;; DEFINE FORM
		 ;;
		 ;; defines either introduce global bindings or initialize static 
		 ;; bindings. They are all turned into set! here
		 ;;
		 [(zodiac:define-values-form? ast)	    
		  (let* ([vars (zodiac:define-values-form-vars ast)]
			 [val (zodiac:define-values-form-val ast)]
			 [body
			  (cond
			   ;; DEFINE STATIC VARREF - written by compiler
			   [(and (varref:has-attribute? (car vars) varref:static) (null? (cdr vars)))
			    (convert val
				     #f
				     (lambda (val)
				       (list
					(make-vm:set! #f
						      (list (cons
							     target-type:static
							     (convert (car vars) #f identity #f #f #t)))
						      val #f)))
				     #f
				     #f
				     #t)]
			   
			   ;; DEFINE GLOBAL VARREFS - user written
			   [(and (andmap zodiac:top-level-varref? vars)
				 (andmap (lambda (v)
					   (not (varref:has-attribute? v varref:static)))
					 vars))
			    (convert val
				     (not (= (length vars) 1))
				     (lambda (val)
				       (list
					(make-vm:set! #f
						      (map (lambda (v)
							     (cons target-type:global
								   (compiler:add-global-varref! v)))
							   vars)
						      val (list "define-values" 1))))
				     #f
				     #f
				     #t)]
			   
			   [else (compiler:internal-error ast "bad define")])])
		    
		    (if tail-pos
			(append! body
				 (leaf (tail-pos 
					(make-vm:immediate #f
							   (zodiac:make-special-constant 'void)))))
			(leaf body)))]

		 
		 ;;-----------------------------------------------------------------
		 ;; DEFINE-SYNTAXES
		 ;;
		 [(zodiac:define-syntaxes-form? ast)	    
		  (let* ([vars (zodiac:define-syntaxes-form-names ast)]
			 [val (zodiac:define-syntaxes-form-expr ast)]
			 [in-mod? (get-annotation ast)]
			 [body
			  (convert val
				   ;; At top level, always multi, because 
				   ;;  zero results => just decl
				   (and in-mod?
					(not (= (length vars) 1)))
				   (lambda (val)
				     (list
				      (make-vm:syntax! #f
						       vars
						       val
						       in-mod?)))
				   #f
				   #f
				   #t)])

		    (if tail-pos
			(append! body
				 (leaf (tail-pos 
					(make-vm:immediate 
					 #f
					 (zodiac:make-special-constant 'void)))))
			(leaf body)))]
		 
		 ;;-------------------------------------------------------------------
		 ;; WITH-CONTINUATION-MARK
		 ;;
		 ;;
		 [(zodiac:with-continuation-mark-form? ast)
		  (let* ([wcm-var (get-annotation ast)]
			 [var (and wcm-var
				   (convert (zodiac:binding->lexical-varref wcm-var)
					    #f
					    identity
					    #f
					    #f
					    #t))]
			 [key (convert (zodiac:with-continuation-mark-form-key ast) #f identity #f #f #t)]
			 [val (convert (zodiac:with-continuation-mark-form-val ast) #f identity #f #f #t)]
			 [body (convert (zodiac:with-continuation-mark-form-body ast)
					multi?
					(lambda (val) 
					  (if var
					      (list (make-vm:wcm-remember!
						     (zodiac:zodiac-stx ast)
						     var val))
					      (leaf val)))
					(and (not wcm-var) tail-pos)
					(and (not wcm-var) tail?)
					used?)]
			 [extract
			  (and wcm-var
			       (make-vm:wcm-extract
				(zodiac:zodiac-stx ast)
				var))]
			 [mark (make-vm:wcm-mark!
				(zodiac:zodiac-stx ast)
				key val)]
			 [push (and wcm-var
				    (make-vm:wcm-push!
				     (zodiac:zodiac-stx ast)
				     var))]
			 [pop (and wcm-var
				   (make-vm:wcm-pop!
				    (zodiac:zodiac-stx ast)
				    var))])
		    (if wcm-var
			
			(append (list push mark)
				body 
				(list pop)
				(if tail-pos
				    (leaf (tail-pos extract))
				    (leaf extract)))
			
			(cons mark body)))]
		 
		 ;;-----------------------------------------------------------------
		 ;; APPLICATIONS
		 ;;
		 ;; distinguish between tail & non-tail calls
		 ;; implement tail calls to "simple" primitives a regular calls
		 ;; no need to pass anything to tail here because it's already
		 ;;  a tail value if its a tail-apply
		 ;; the vm-optimizer will refine the multi-ness of this application,
		 ;;  and worry about inter & intra-vehicle calls
		 ;;
		 [(zodiac:app? ast)
		  (unless (eq? tail? (app-tail? (get-annotation ast))) 
		    (compiler:internal-error 
		     ast 
		     "vmscheme: annotated tail? (= ~a) does not match calculated tail? (= ~a)"
		     (app-tail? (get-annotation ast)) tail?))
		  (let* ([prim (let* ([fun (zodiac:app-fun ast)]
				      [name (and (zodiac:varref? fun)
						 (zodiac:varref-var fun))])
				 (and (zodiac:top-level-varref? fun)
				      (or (and (varref:has-attribute? fun varref:primitive)
					       (let ([v (dynamic-require 'mzscheme name)])
						 (or (primitive? v) 
						     (primitive-closure? v)))
					       (zodiac:varref-var fun))
					  (and (identifier? (zodiac:zodiac-stx fun))
					       (module-identifier=? (zodiac:zodiac-stx fun) 
								    for-syntax-in-env-stx)
					       'for-syntax-in-env))))]
			 [simple-tail-prim? (and tail? (simple-tail-prim? prim))]
			 [closure (convert (zodiac:app-fun ast) #f identity #f #f #t)]
			 [args (zodiac:app-args ast)]
			 [argc (length args)]
			 [converted-args
			  (map (lambda (A)
				 (convert A #f identity #f #f #t))
			       args)])
		    (check-primitive-as-macro
		     prim argc
		     (lambda (special bool?)
		       (let* ([arg-locals (map (lambda (x)
						 (let* ([name (gensym 'macapply)]
							[b (zodiac:make-binding 
							    #f
							    (make-empty-box)
							    name name)])
						   (set-annotation! b
								    (make-binding #f #t #f #f #f #f #f #f #f
										  (make-rep:atomic 'scheme-object)))
						   b))
					       args)])
			 (for-each add-new-local! arg-locals)
			 (cons (make-vm:register-args (zodiac:zodiac-stx ast)
						      arg-locals
						      converted-args)
			       (leaf (make-vm:macro-apply (zodiac:zodiac-stx ast)
							  special
							  closure
							  arg-locals
							  tail?
							  magic?
							  bool?)))))
		     (lambda ()
		       (cons (make-vm:generic-args (zodiac:zodiac-stx ast)
						   closure
						   (and tail? (not simple-tail-prim?))
						   magic?
						   prim
						   converted-args)
			     (if tail?
				 (if simple-tail-prim?
				     (leaf (make-vm:apply
					    (zodiac:zodiac-stx ast)
					    closure 
					    argc
					    #f 
					    #t ; tail-call: multi always ok
					    prim
					    #t))
				     (leaf (make-vm:tail-apply
					    (zodiac:zodiac-stx ast)
					    closure
					    argc
					    prim)))
				 (leaf (make-vm:apply
					(zodiac:zodiac-stx ast)
					closure 
					argc
					#f 
					multi?
					prim
					#f)))))))]
		 
		 
		 ;;-----------------------------------------------------------------
		 ;; VARREFS
		 ;;
		 ;; Variables might be boxes, in which case we must turn
		 ;; them into derefs
		 ;; Env-varrefs have already been pulled into registers for us
		 ;; varrefs might be in tail position, so we need to convert them
		 ;; to tail statements.
		 ;;
		 [(zodiac:bound-varref? ast)
		  (let ([vm-ref (vm:convert-bound-varref ast)])
		    (if tail-pos
			(leaf (tail-pos vm-ref))
			(leaf vm-ref)))]
		 
		 ;; will change when representations can be chosen 
		 ;; for static variables
		 [(zodiac:top-level-varref? ast)
		  (let* ([ignore-ast (lambda (maker)
				       (lambda (a d ast)
					 (maker a d)))]
			 [convert-global (lambda (maker)
					   (lambda (a d ast)
					     (maker a (compiler:add-global-varref! ast))))]
			 [maker 
			  (cond
			   [(top-level-varref/bind-from-lift? ast)
			    (lambda (a d ast)
			      ((if (top-level-varref/bind-from-lift-pls? ast)
				   make-vm:per-load-static-varref-from-lift
				   make-vm:static-varref-from-lift)
			       a d (top-level-varref/bind-from-lift-lambda ast)))]
			   [(varref:has-attribute? ast varref:per-load-static)
			    (ignore-ast make-vm:per-load-static-varref)]
			   [(varref:has-attribute? ast varref:primitive)
			    (convert-global make-vm:primitive-varref)]
			   [(varref:has-attribute? ast varref:symbol)
			    (ignore-ast make-vm:symbol-varref)]
			   [(varref:has-attribute? ast varref:inexact)
			    (ignore-ast make-vm:inexact-varref)]
			   [(varref:has-attribute? ast varref:static)
			    (ignore-ast make-vm:static-varref)]
			   [else
			    (convert-global make-vm:global-varref)])])
		    (let ([ref (maker #f (zodiac:varref-var ast) ast)])
		      (if tail-pos
			  (leaf (tail-pos ref))
			  (leaf ref))))]
		 
		 ;;-----------------------------------------------------------------
		 ;; CONSTANTS
		 ;;
		 [(zodiac:quote-form? ast)
		  (let* ([const (zodiac:quote-form-expr ast)]
			 [stx (or (zodiac:zodiac-stx ast) (zodiac:zodiac-stx const))]
			 [vm ((if (vm:literal-constant? const) 
				  (lambda (n)
				    (make-vm:immediate stx n))
				  (lambda (n)
				    (make-vm:build-constant stx n)))
			      const)])
		    (if tail-pos
			(leaf (tail-pos vm))
			(leaf vm)))]

		 ;;-----------------------------------------------------------
		 ;; GLOBALS
		 ;;
		 [(zodiac:global-prepare? ast)
		  (let ([expr (make-vm:global-prepare 
			       (zodiac:zodiac-stx ast)
			       (convert (zodiac:global-prepare-vec ast) #f identity #f #f #t)
			       (zodiac:global-prepare-pos ast))])
		    (if tail-pos
			(leaf (tail-pos expr))
			(leaf expr)))]
		 [(zodiac:global-lookup? ast)
		  (let ([expr (make-vm:global-lookup 
			       (zodiac:zodiac-stx ast)
			       (convert (zodiac:global-lookup-vec ast) #f identity #f #f #t)
			       (zodiac:global-lookup-pos ast))])
		    (if tail-pos
			(leaf (tail-pos expr))
			(leaf expr)))]
		 [(zodiac:global-assign? ast)
		  (let ([expr (make-vm:global-assign 
			       (zodiac:zodiac-stx ast)
			       (convert (zodiac:global-assign-vec ast) #f identity #f #f #t)
			       (convert (zodiac:global-assign-expr ast) #f identity #f #f #t)
			       (zodiac:global-assign-pos ast))])
		    (if tail-pos
			(leaf (tail-pos expr))
			(leaf expr)))]
		 [(zodiac:safe-vector-ref? ast)
		  (let ([expr (make-vm:safe-vector-ref 
			       (zodiac:zodiac-stx ast)
			       (convert (zodiac:safe-vector-ref-vec ast) #f identity #f #f #t)
			       (zodiac:safe-vector-ref-pos ast))])
		    (if tail-pos
			(leaf (tail-pos expr))
			(leaf expr)))]

		 [else 
		  (compiler:internal-error 
		   ast 
		   (format "vm-phase: form not supported ~a" ast))]))])
	  
	  (begin
	    (set! new-locals empty-set)
	    ;; l->r evaluation necessary for convert to get called before new-locals
	    ;; is evaluated
	    (values (make-vm:sequence
		     (zodiac:zodiac-stx ast)
		     (convert ast
			      multi? (or leaf list) tail-pos tail? (not tail?)))
		    new-locals))))))
