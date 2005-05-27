;; VM Optimization pass
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-201 PLT

;; This pass only allows T & V statements to be expanded into multiple
;; statments there is not a mechanism to expand R, A, or L
;; expressions. (See vmscheme.ss.)

(module vmopt mzscheme
  
  (require (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide vmopt@)
  (define vmopt@
    (unit/sig
	compiler:vmopt^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:vmstructs^
	      compiler:known^
	      compiler:rep^
	      compiler:vmphase^
	      compiler:driver^)

      (define satisfies-arity?
	(lambda (arity L arglist)
	  (let-values ([(min-arity max-arity) (compiler:formals->arity*
					       (if arglist
						   (list arglist)
						   (zodiac:case-lambda-form-args L)))])
	    (if (= -1 max-arity)
		(>= arity min-arity)
		(= arity min-arity)))))

      (define (select-case L argc)
	(if L
	    (let loop ([args (zodiac:case-lambda-form-args L)][i 0])
	      (cond
	       [(null? args) (values #f #f)]
	       [(satisfies-arity? argc L (car args)) (values i (car args))]
	       [else (loop (cdr args) (add1 i))]))
	    (values #f #f)))

      (define (case-label L label case)
	(if (= 1 (length (zodiac:case-lambda-form-args L)))
	    label
	    (begin
	      (unless case
		(compiler:internal-error 
		 #f 
		 (format "vm-optimize: bad case label ~a" case)))
	      (cons label case))))

      (define a-val/l-val/immediate? (one-of vm:global-varref? vm:primitive-varref? vm:local-varref? 
					     vm:symbol-varref? vm:inexact-varref? 
					     vm:static-varref? vm:bucket?
					     vm:per-load-statics-table? vm:per-invoke-statics-table?
					     vm:struct-ref? vm:deref? vm:immediate?))

      (define vm-optimize!
	(lambda (current-lambda current-case)
	  (letrec ([closure-info
		    (lambda (closure)
		      (let* ([L #f]
			     [closure-label
			      (let loop ([closure closure])
				(cond
				 [(or (vm:local-varref? closure)
				      (vm:static-varref-from-lift? closure)
				      (vm:per-load-static-varref-from-lift? closure)
				      (vm:per-invoke-static-varref-from-lift? closure))
				  (let ([known 
					 (cond
					  [(vm:local-varref? closure) (extract-varref-known-val 
								       (vm:local-varref-binding closure))]
					  [(vm:static-varref-from-lift? closure)
					   (vm:static-varref-from-lift-lambda closure)]
					  [(vm:per-load-static-varref-from-lift? closure)
					   (vm:per-load-static-varref-from-lift-lambda closure)]
					  [else
					   (vm:per-invoke-static-varref-from-lift-lambda closure)])])
				    (and known
					 (zodiac:case-lambda-form? known)
					 (begin (set! L known) #t)
					 (closure-code-label 
					  (get-annotation known))))]
				 [(vm:deref? closure) (loop (vm:deref-var closure))]
				 [else #f]))])
			(values L closure-label)))]
		   
		   ;; This takes action based on the label associated with the closure 
		   ;; passed in. There is a HACK here.  The 'known' value of this lexical
		   ;; varref is a lambda, even though we have eliminated lambda.
		   [with-closure
		    (lambda (closure closure-case unknown call recur)
		      (let-values ([(L closure-label) (closure-info closure)])
			(let ([same-vehicle? 
			       (and L
				    current-vehicle
				    (= current-vehicle 
				       (closure-code-vehicle (get-annotation L))))])
			  ((cond
			    [(not closure-label) unknown]
			    [(not current-lambda) call]
			    [(not current-label) call]
			    [(not current-vehicle) call]
			    [(and same-vehicle? (= closure-label current-label) (= closure-case current-case)) recur]
			    [else call])
			   closure-label 
			   closure-case
			   L
			   same-vehicle?
			   ))))]
		   [current-label
		    (and current-lambda
			 (closure-code-label (get-annotation current-lambda)))]
		   [current-vehicle
		    (and current-lambda
			 (closure-code-vehicle (get-annotation current-lambda)))]
		   [new-locs empty-set]
		   [add-local-var!
		    (lambda (binding)
		      (set! new-locs (set-union-singleton new-locs binding)))]
		   [process!
		    (lambda (ast)
		      (cond
		       
		       ;;====================================================================
		       ;; BLOCK STATMENTS (B & S)
		       
		       ;;--------------------------------------------------------------------
		       ;; SEQUENCE STATMENTS
		       ;;
		       ;; very simple.  gather the transformations for each of the 
		       ;; instructions weave them back together into one sequence
		       ;;
		       [(vm:sequence? ast)
			(set-vm:sequence-vals! ast
					       (apply append! 
						      (map process! (vm:sequence-vals ast))))
			ast]

		       [(vm:module-body? ast)
			(set-vm:module-body-vals! ast
						  (apply append! 
							 (map process! (vm:module-body-vals ast))))
			ast]
		       
		       ;;--------------------------------------------------------------------
		       ;; IF STATEMENTS
		       ;;
		       ;; to reduce the nesting of ifs, especially in functional code, we
		       ;; do the following optimization
		       ;; (if A (sequence ... (RET X)) B) -->
		       ;; (if A (sequence ... (RET X))), B
		       ;; where RET is any instruction that terminates control such as
		       ;; a return, tail-call, etc.
		       ;;
		       [(vm:if? ast)
			(let*-values ([(test) (apply append (map process! (vm:if-test ast)))]
				      [(test-setup test) (let loop ([l test][acc null])
							   (if (null? (cdr l))
							       (values (reverse! acc) (car l))
							       (loop (cdr l) (cons (car l) acc))))])
			  (append
			   test-setup
			   (begin
			     (set-vm:if-test! ast test)
			     (set-vm:if-then! ast (process! (vm:if-then ast)))
			     (set-vm:if-else! ast (process! (vm:if-else ast)))
			     (let* ([seq (vm:sequence-vals (vm:if-then ast))]
				    [last (and (pair? seq) ; optimizations can make it null
					       (list-last seq))])
			       (if (vm:control-return? last)
				   (begin0
				    (cons ast (vm:sequence-vals (vm:if-else ast)))
				    (set-vm:if-else! ast (make-vm:sequence #f '())))
				   (list ast))))))]

		       
		       ;;--------------------------------------------------------------------
		       ;; BEGIN0 STATMENTS
		       ;;
		       ;;
		       [(vm:begin0-mark!? ast)
			(set-vm:begin0-mark!-var! ast (car (process! (vm:begin0-mark!-var ast))))
			(set-vm:begin0-mark!-val! ast (car (process! (vm:begin0-mark!-val ast))))
			(list ast)]

		       [(vm:begin0-setup!? ast)
			(set-vm:begin0-setup!-var! ast (car (process! (vm:begin0-setup!-var ast))))
			(list ast)]

		       [(vm:begin0-extract? ast)
			(set-vm:begin0-extract-var! ast (car (process! (vm:begin0-extract-var ast))))
			(list ast)]

		       ;;====================================================================
		       ;; TAIL POSITION STATEMENTS
		       
		       ;;--------------------------------------------------------------------
		       ;; VOID STATEMENT
		       ;;
		       ;; with dead code flags, we could throw it out
		       ;;
		       [(vm:void? ast)
			(let ([val (car (process! (vm:void-val ast)))])
			  (if (vm:immediate? val)
			      null
			      (begin
				(set-vm:void-val! ast val)
				(list ast))))]
		       
		       ;;--------------------------------------------------------------------
		       ;; RETURN STATEMENT
		       ;; 
		       [(vm:return? ast) 
			(set-vm:return-val! ast (car (process! (vm:return-val ast))))
			(list ast)]
		       
		       ;;--------------------------------------------------------------------
		       ;; TAIL-APPLY STATEMENT
		       ;; 
		       ;; if this is to a known function, turn this into a tail CALL
		       ;; or if it is a tail-recursion, turn into a CONTINUE
		       ;;
		       [(vm:tail-apply? ast)
			(list
			 (let*-values ([(closure) (vm:tail-apply-closure ast)]
				       [(L closure-label) (closure-info closure)]
				       [(cl-case arglist) (select-case L (vm:tail-apply-argc ast))])
			   (if (and L (not (and cl-case
						(zodiac:list-arglist?  arglist)
						(satisfies-arity? (vm:tail-apply-argc ast) L arglist))))
			       
			       ast
			       
			       (with-closure 
				closure
				cl-case
				;; unknown tail call site
				(lambda (_ __ ___ ____) ast)
				
				;; known tail call site
				;; if the environment is empty, allow the backend to
				;; eliminate the env-setting instruction
				(lambda (label cl-case _ same-vehicle?)
				  (let* ([code (get-annotation L)]
					 [free-vars (code-free-vars code)]
					 [global-vars (code-global-vars code)])
				    (if same-vehicle?
					(make-vm:tail-call
					 (zodiac:zodiac-stx ast)
					 (case-label L label cl-case)
					 closure
					 (or (not (set-empty? free-vars))
					     (not (set-empty? global-vars))))
					ast)))
				
				;; known tail recursion site
				(lambda (label cl-case _ __)
				  (if (zodiac:list-arglist? arglist)
				      (begin
					;; Mark the case as having a continue
					(set-case-code-has-continue?! 
					 (list-ref (procedure-code-case-codes (get-annotation L)) cl-case) 
					 #t)
					(make-vm:continue (zodiac:zodiac-stx ast)))
				      (make-vm:tail-call (zodiac:zodiac-stx ast)
							 (case-label L label cl-case)
							 closure)))))))]
		       
		       ;;====================================================================
		       ;; NON-TAIL POSITION STATEMENTS
		       
		       ;;--------------------------------------------------------------------
		       ;; SET! STATEMENTS
		       ;;
		       ;; if this binds multiple values, be sure the apply on the other end
		       ;; is a multi-apply
		       ;;
		       [(vm:set!? ast)
			(when (vm:apply? (vm:set!-val ast))
			  (set-vm:apply-multi?!
			   (vm:set!-val ast)
			   (not (= 1 (length (vm:set!-vars ast))))))
			(set-vm:set!-val! ast (car (process! (vm:set!-val ast))))
			(list ast)]
		       
		       ;;--------------------------------------------------------------------
		       ;; ARGS
		       ;;
		       ;; We implement a mapping of many types of function calls to 3 arg 
		       ;; types and check for arity if the call is to a known function
		       ;; 
		       ;;
		       [(vm:generic-args? ast)
			(if (vm:generic-args-prim ast)
			    (list (make-vm:args (zodiac:zodiac-stx ast)
						(if (vm:generic-args-tail? ast)
						    arg-type:tail-arg
						    arg-type:arg)
						(vm:generic-args-vals ast)))
			    (let*-values ([(L closure-label) 
					   (closure-info (vm:generic-args-closure ast))]
					  [(tail?) (vm:generic-args-tail? ast)]
					  [(vals) (vm:generic-args-vals ast)]
					  [(cl-case arglist) (select-case L (length (vm:generic-args-vals ast)))])
			      (if (and closure-label
				       cl-case
				       (zodiac:list-arglist? arglist))
				  
				  ;; known function, fixed arity
				  (if (not (satisfies-arity? (length (vm:generic-args-vals ast)) 
							     L arglist))
				      (begin 
					((if (compiler:option:stupid) compiler:warning compiler:error )
					 ast 
					 "procedure called with wrong number of arguments")
					(list (make-vm:args (zodiac:zodiac-stx ast)
							    (if tail?
								arg-type:tail-arg
								arg-type:arg)
							    vals)))
				      
				      (with-closure 
				       (vm:generic-args-closure ast)
				       cl-case

				       ;; unknown function - could be at a level where an 
				       ;; optimized jump is not allowed
				       (lambda (_ __ ___ ____)
					 (list (make-vm:args (zodiac:zodiac-stx ast)
							     (if tail? 
								 arg-type:tail-arg
								 arg-type:arg)
							     vals)))
				       
				       ;; known call
				       (lambda (label cl-case L same-vehicle?)
					 (list (make-vm:args (zodiac:zodiac-stx ast)
							     (if tail?
								 (if same-vehicle?
								     arg-type:register
								     arg-type:tail-arg)
								 arg-type:arg)
							     vals)))
				       
				       ;; known recursion
				       ;; tail recursion we just optimize to 
				       ;; set! of local variables
				       (lambda (label cl-case L _)
					 (if (not tail?)
					     (list (make-vm:args (zodiac:zodiac-stx ast)
								 arg-type:arg vals))
					     (let ([bindings (zodiac:arglist-vars 
							      (list-ref (zodiac:case-lambda-form-args L) 
									cl-case))])
					       (let loop ([bindings bindings][vals vals][set-ok? #f])
						 (if (null? bindings)
						     null
						     (let* ([binding (car bindings)]
							    [val (car vals)]
							    [this-binding? 
							     (lambda (val)
							       (let loop ([val val])
								 (or (and (vm:local-varref? val)
									  (eq? (vm:local-varref-binding val)
									       binding))
								     (and (vm:deref? val)
									  (loop (vm:deref-var val))))))])
						       ;; If this is x = x, skip it.
						       (if (this-binding? val)
							   (loop (cdr bindings) (cdr vals) #f)
							   
							   ;; Check whether the binding we're about to set is needed later as a value.
							   ;; If so, invent a new register
							   (if (and (not set-ok?)
								    (ormap this-binding? (cdr vals)))
							       (let* ([rep (binding-rep (get-annotation binding))]
								      [name (gensym)]
								      [new-binding (let ([b (zodiac:make-binding 
											     #f
											     (make-empty-box)
											     name name)])
										     (set-annotation! b
												      (make-binding #f #t #f #f #f #f #f #f #f
														    (if (rep:pointer? rep)
															(rep:pointer-to rep)
															rep)))
										     b)]
								      [v (make-vm:local-varref #f name new-binding)])
								 (add-local-var! new-binding)
								 ;; Start over; replace uses of binding in vals with uses of new-binding
								 (loop (cons new-binding bindings)
								       (list* (let ([v (make-vm:local-varref 
											#f
											(zodiac:binding-var binding)
											binding)])
										(if (rep:pointer? rep)
										    (make-vm:deref #f v)
										    v))
									      (car vals)
									      (map
									       (lambda (val)
										 (if (this-binding? val)
										     v
										     val))
									       (cdr vals)))
								       #t))
							       
							       ;; Normal set
							       (let*-values ([(vref) 
									      (zodiac:binding->lexical-varref binding)]
									     [(vm _) (vm-phase vref #f #f identity #f)]
									     [(vm) (car (vm:sequence-vals vm))])
								 (cons (make-vm:set! 
									(zodiac:zodiac-stx val)
									(list
									 (cons target-type:lexical vm))
									val
									#f)
								       (loop (cdr bindings) (cdr vals) #f)))))))))))))
				  
				  ;; unknown or variable arity function call - always use args
				  (if (or (not closure-label)
					  (and closure-label (satisfies-arity? (length vals) 
									       L arglist)))
				      
				      (list (make-vm:args (zodiac:zodiac-stx ast)
							  (if tail?
							      arg-type:tail-arg
							      arg-type:arg)
							  vals))
				      
				      (begin
					((if (compiler:option:stupid) compiler:warning compiler:error)
					 ast
					 "procedure called with wrong number of arguments")
					(list (make-vm:args (zodiac:zodiac-stx ast)
							    (if tail?
								arg-type:tail-arg
								arg-type:arg)
							    vals)))))))]
		       
		       ;;--------------------------------------------------------------------
		       ;; ARGS
		       ;;
		       ;; args that have already been assigned to register variables
		       ;;
		       [(vm:register-args? ast) (list ast)]

		       ;;--------------------------------------------------------------------
		       ;; SYNTAX! DEFINITIONS
		       [(vm:syntax!? ast) 
			(set-vm:syntax!-val! ast (car (process! (vm:syntax!-val ast))))
			(list ast)]
		       
		       ;;====================================================================
		       ;; R-VALUES (ONE STEP COMPUTATIONS)
		       
		       ;;--------------------------------------------------------------------
		       ;; ALLOC EXPRESSION
		       ;;
		       [(vm:alloc? ast) (list ast)]
		       
		       ;;--------------------------------------------------------------------
		       ;; BUILD-CONSTANT
		       ;;
		       [(vm:build-constant? ast) (list ast)]
		       
		       ;;--------------------------------------------------------------------
		       ;; MAKE-CLOSURE, all kinds (wrap closure?)
		       ;;
		       [(vm:make-closure? ast) 
			(set-vm:make-closure-closure! 
			 ast
			 (let ([cc (vm:make-closure-closure ast)])
			   (and cc (car (process! cc)))))
			(list ast)]
		       
		       ;;--------------------------------------------------------------------
		       ;; APPLY EXPRESSION
		       ;;
		       ;; check for primitive applications
		       ;; check for variable arity applications
		       ;; check for applications that can return multiple values
		       ;;
		       [(vm:apply? ast)
			(if (not (vm:apply-prim ast))
			    (let*-values ([(closure) (vm:apply-closure ast)]
					  [(L closure-label) (closure-info closure)]
					  [(cl-case arglist) (select-case L (vm:apply-argc ast))]
					  [(check-known-sv)
					   (lambda ()
					     (when (not (closure-code-return-multi
							 (get-annotation L)))
					; Known proc returns a single value, so we can
					; use the more efficient multi call form
					       (set-vm:apply-multi?! ast #t)))])
			      (if (or (not cl-case)
				      (and cl-case (not (zodiac:list-arglist? arglist)))
				      (and cl-case (not (satisfies-arity? (vm:apply-argc ast) L arglist))))
				  (list ast)
				  
				  (with-closure closure
						cl-case
						
					; unknown application
						(lambda (_ __ ___ ____) (list ast))
						
					; known call
						(lambda (label _ __ ___) 
						  (check-known-sv)
						  (set-vm:apply-known?! ast #t)
						  (list ast))
						
					; known recursion
						(lambda (label _ __ ____) 
						  (check-known-sv)
						  (set-vm:apply-known?! ast #t)
						  (list ast)))))
			    (list ast))]


		       [(vm:macro-apply? ast) (list ast)]

		       ;;--------------------------------------------------------------------
		       ;; MODULE CONSTRUCTION
		       ;;
		       [(vm:module-create? ast) (list ast)]
		       
		       ;;--------------------------------------------------------------------
		       ;; WITH-CONTINUATION-MARK
		       ;;
		       [(vm:wcm-mark!? ast)
			(set-vm:wcm-mark!-key! ast (car (process! (vm:wcm-mark!-key ast))))
			(set-vm:wcm-mark!-val! ast (car (process! (vm:wcm-mark!-val ast))))
			(list ast)]

		       [(vm:wcm-push!? ast)
			(set-vm:wcm-push!-var! ast (car (process! (vm:wcm-push!-var ast))))
			(list ast)]

		       [(vm:wcm-pop!? ast)
			(set-vm:wcm-pop!-var! ast (car (process! (vm:wcm-pop!-var ast))))
			(list ast)]
		       
		       [(vm:wcm-remember!? ast)
			(set-vm:wcm-remember!-var! ast (car (process! (vm:wcm-remember!-var ast))))
			(set-vm:wcm-remember!-val! ast (car (process! (vm:wcm-remember!-val ast))))
			(list ast)]
		       
		       [(vm:wcm-extract? ast)
			(set-vm:wcm-extract-var! ast (car (process! (vm:wcm-extract-var ast))))
			(list ast)]
		       
		       ;;====================================================================
		       ;; A-VALUES, L-VALUES, IMMEDIATES

		       [(a-val/l-val/immediate? 
			 ast)
			(list ast)]
		       
		       [else
			(compiler:internal-error 
			 #f 
			 (format "vm-optimize: unrecognized form ~a" ast))]))])
	    (lambda (ast)
	      (set! new-locs empty-set)
	      (values
	       (process! ast)
	       new-locs))))))))
