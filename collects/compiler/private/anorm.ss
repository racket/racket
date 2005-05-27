;; A-Normalizer
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

; This file contains an "a-normalizer" for Zodiac abstract
;  syntax trees for Scheme.
; This linear time algorithm is adapted from "The Essence
;  of Compiling with Continuations"(Flanagan/Sabry/Duba/Felleisen)

; For unknown historical reasons, this phase is implemented as a
;  non-destructive procedure on ASTs.

; An expressions is given a name when
;  1) it is not already the RHS of a let-assignment
;  2) it is not a tail expression
;  3) the value is not known to be ignored
; There's also a special hack for the test part of an
;  `if' expression: it might be preserved as an
;  application inlined in the `if' form.

; After a-normalizations, all let expressions are "linearized": one
;  binding clause for each let-values expression. (Of course, the
;  single clause can bind multiple variables.) This linearization does
;  not apply to letrec expressions.

;;; Annotatitons: ----------------------------------------------
;;    begin0 - lexical-binding for storing 0th expression result
;;    with-continuation-mark - lexical-binding for storing body
;;                             result
;;; ------------------------------------------------------------

(module anorm mzscheme
  (require (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide anorm@)
  (define anorm@
    (unit/sig
	compiler:anorm^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:driver^)

      (define compiler:a-value?
	(one-of zodiac:quote-form? zodiac:varref? zodiac:quote-syntax-form?))

      (define a-normalize
	(letrec ([linearize-let-values
		  (lambda (ast)
		    (let ([vars (zodiac:let-values-form-vars ast)])
		      (cond
		       [(null? (cdr vars)) ast] ; to prevent N^2 behavior
		       [else
			(let linear ([vars vars]
				     [vals (zodiac:let-values-form-vals ast)])
			  (if (null? vars)
			      (zodiac:let-values-form-body ast)
			      (zodiac:make-let-values-form (zodiac:zodiac-stx ast)
							   (make-empty-box)
							   (list (car vars))
							   (list (car vals))
							   (linear (cdr vars) 
								   (cdr vals)))))])))]
		 [normalize-name
		  (lambda (ast k)
		    (normalize-name/special-a-values ast k (lambda (x) #f)))]
		 [normalize-name/special-a-values
		  ;; The magic goodie that names expressions.  If the expression
		  ;; handed in is not an immediate a-value, it is named and the
		  ;; computation continues; syntax correlation exists!
		  (lambda (ast k special-a-value?)
		    (a-normalize
		     ast
		     (lambda (exp)
		       (if (or (compiler:a-value? exp) (special-a-value? exp))
			   (k exp)
			   (let* ([tname (gensym)]
				  [tbound (zodiac:make-lexical-binding
					   (zodiac:zodiac-stx exp)
					   (make-empty-box)
					   tname
					   tname)]
				  [varref (zodiac:binding->lexical-varref tbound)])
			     ;; hack: #f annotation => not mutable, or anything else
			     ;; (The hack is resolved by the prephase:is-mutable?, etc.
			     ;; procedures.)
			     (set-annotation! tbound #f) 
			     (let ([body (k varref)])
			       (zodiac:make-let-values-form
				(zodiac:zodiac-stx exp)
				(make-empty-box)
				(list (list tbound))
				(list exp)
				body)))))))]
		 ;; This names a list of expressions (eg argument list)
		 [normalize-name*
		  (lambda (ast* k)
		    (if (null? ast*)
			(k null)
			(normalize-name
			 (car ast*)
			 (lambda (term)
			   (normalize-name* (cdr ast*)
					    (lambda (term*)
					      (k (cons term term*))))))))]


		 [a-normalize
		  (lambda (ast k)
		    (when (compiler:option:debug)
		      (zodiac:print-start! (debug:get-port) ast)
		      (newline (debug:get-port)))
		    (cond 
		     
		     ;;----------------------------------------------------------------
		     ;; LAMBDA EXPRESSIONS
		     ;;    We must make a recursive call to normalize the body.
		     ;;    Otherwise, we just pass them on.  Lambda must be queried
		     ;;    before a-value, since lambda might be an a-value
		     ;;
		     ;; (norm (lambda x M)) -> (lambda x (norm M))
		     ;;
		     [(zodiac:case-lambda-form? ast)
		      (k (zodiac:make-case-lambda-form
			  (zodiac:zodiac-stx ast)
			  (zodiac:parsed-back ast)
			  (zodiac:case-lambda-form-args ast)
			  (map (lambda (body)
				 (a-normalize body identity))
			       (zodiac:case-lambda-form-bodies ast))))]
		     
		     ;;--------------------------------------------------------------
		     ;; A-VALUES
		     ;;    a-values are passed along unharmed.  We have to handle
		     ;;    lambda separately above, but otherwise
		     ;;
		     ;; (norm a-value) -> a-value
		     ;;
		     [(compiler:a-value? ast) (k ast)]
		     
		     ;;--------------------------------------------------------------
		     ;; LET EXPRESSIONS
		     ;;    with let, we must normalize the bound expressions
		     ;;    as well as the body.  We only bind one variable per
		     ;;    let in Core Scheme, so we have to expand these out
		     ;;    Zodiac already tells us if something is unbound, so we
		     ;;    can linearize this let as we like.
		     ;;
		     ;;    we treat letrec separately to reduce the cost of 
		     ;;    optimization
		     ;;    later.  We don't have to look for special cases of set!
		     ;;    we do not guarantee a-values in the vals slot of the letrec
		     ;;    since we do each of those in its own context, otherwise we
		     ;;    can get bindings messed up.
		     ;;
		     ;; (norm (let x M B) k) -> 
		     ;;       (norm M (lambda V (let x V (norm B k))))
		     ;; (norm (letrec [x M] ... B)) -> 
		     ;;     (letrec [x (norm M)] ... (norm B))
		     ;;
		     [(zodiac:let-values-form? ast)
		      (if (null? (zodiac:let-values-form-vars ast))
			  (a-normalize (zodiac:let-values-form-body ast) k)
			  (let ([linear (linearize-let-values ast)])
			    (a-normalize
			     (car (zodiac:let-values-form-vals ast))
			     (lambda (V)
			       (zodiac:make-let-values-form 
				(zodiac:zodiac-stx linear)
				(zodiac:parsed-back linear)
				(zodiac:let-values-form-vars 
				 linear)
				(list V)
				(a-normalize 
				 (zodiac:let-values-form-body 
				  linear)
				 k))))))]
		     
		     [(zodiac:letrec-values-form? ast)
		      (let ([vals (map (lambda (val) (a-normalize val identity))
				       (zodiac:letrec-values-form-vals ast))])
			(zodiac:make-letrec-values-form
			 (zodiac:zodiac-stx ast)
			 (zodiac:parsed-back ast)
			 (zodiac:letrec-values-form-vars ast)
			 vals
			 (a-normalize (zodiac:letrec-values-form-body ast) k)))]
		     
		     ;;---------------------------------------------------------------
		     ;; IF EXPRESSIONS
		     ;;
		     ;; We do not make a recursive call for the test since it is in the
		     ;; current 'context'.  We want only a-values in the test slot,
		     ;; or an application of a primitive function to a-values. 
		     ;;
		     ;; We specially allow primitive applications
		     ;; of a-values so the optimizer can recognize tests that can be
		     ;; implemented primitively, e.g., (zero? x)
		     ;;
		     ;; (norm (if A B C) k) ->
		     ;;   (name A (lambda test (k (if test (norm B) (norm C)))))
		     ;;
		     [(zodiac:if-form? ast)
		      (normalize-name/special-a-values
		       (zodiac:if-form-test ast)
		       (lambda (test)
			 (k (zodiac:make-if-form (zodiac:zodiac-stx ast)
						 (zodiac:parsed-back ast)
						 test    
						 (a-normalize (zodiac:if-form-then ast)
							      identity)
						 (a-normalize (zodiac:if-form-else ast)
							      identity))))
		       (lambda (x)
			 (and (zodiac:app? x)
			      (let ([fun (zodiac:app-fun x)])
				(and (zodiac:top-level-varref? fun)
				     (varref:has-attribute? fun varref:primitive))))))]
		     
		     ;;----------------------------------------------------------------
		     ;; BEGIN EXPRESSIONS
		     ;;
		     ;;    Begins pass through as begins, but every body is 
		     ;;    a-normalized.
		     ;;    We are guaranteed no empty begins
		     ;;
		     ;; (norm (begin A B) k) ->
		     ;;    (norm A (lambda first (begin first (norm B k))))
		     ;;
		     [(zodiac:begin-form? ast)   
		      (k (zodiac:make-begin-form
			  (zodiac:zodiac-stx ast)
			  (zodiac:parsed-back ast)
			  (map (lambda (b) (a-normalize b identity)) 
			       (zodiac:begin-form-bodies ast))))]
		     
		     ;;----------------------------------------------------------------
		     ;; BEGIN0 EXPRESSIONS
		     ;;
		     ;;    The first is named in a special way, and the rest passes through
		     ;;
		     ;; (norm (begin0 A B) k) ->
		     ;;    (k (begin0 (norm A identity) (norm B identity)))
		     ;;
		     [(zodiac:begin0-form? ast)
		      (let* ([tname (gensym)]
			     [tbound (zodiac:make-lexical-binding
				      (zodiac:zodiac-stx ast)
				      (make-empty-box)
				      tname
				      tname)]
			     [begin0-exp
			      (zodiac:make-begin0-form
			       (zodiac:zodiac-stx ast)
			       (zodiac:parsed-back ast)
			       (list
				(a-normalize (zodiac:begin0-form-first ast) identity)
				(a-normalize (zodiac:begin0-form-rest ast) identity)))])
			(set-annotation! begin0-exp tbound)
			(k begin0-exp))]

		     ;;-----------------------------------------------------------
		     ;; MODULE
		     ;;
		     [(zodiac:module-form? ast)
		      (k (zodiac:make-module-form
			  (zodiac:zodiac-stx ast)
			  (zodiac:parsed-back ast)
			  (zodiac:module-form-name ast)
			  (zodiac:module-form-requires ast)
			  (zodiac:module-form-for-syntax-requires ast)
			  (zodiac:module-form-for-template-requires ast)
			  (a-normalize (zodiac:module-form-body ast) identity)
			  #f ; see split-module in driver.ss
			  (zodiac:module-form-provides ast)
			  (zodiac:module-form-syntax-provides ast)
			  (zodiac:module-form-indirect-provides ast)
			  (zodiac:module-form-kernel-reprovide-hint ast)
			  (zodiac:module-form-self-path-index ast)))]

		     ;;---------------------------------------------------------------
		     ;; SET! EXPRESSIONS / DEFINE EXPRESSIONS
		     ;;    
		     ;; (norm (set! x M)) -> (name M (lambda val (set! x M)))
		     ;; (norm (define x M))->(define x (norm M identity))
		     ;;
		     [(zodiac:set!-form? ast)
		      (normalize-name 
		       (zodiac:set!-form-val ast)
		       (lambda (norm-val)
			 (k (zodiac:make-set!-form
			     (zodiac:zodiac-stx ast)
			     (zodiac:parsed-back ast)
			     (zodiac:set!-form-var ast)
			     norm-val))))]
		     
		     [(zodiac:define-values-form? ast)
		      (k (zodiac:make-define-values-form
			  (zodiac:zodiac-stx ast)
			  (zodiac:parsed-back ast)
			  (zodiac:define-values-form-vars ast)
			  (a-normalize (zodiac:define-values-form-val ast) identity)))]
		     
		     ;;----------------------------------------------------------
		     ;; DEFINE-SYNTAX
		     ;;
		     [(zodiac:define-syntaxes-form? ast)
		      (k (zodiac:make-define-syntaxes-form 
			  (zodiac:zodiac-stx ast)
			  (zodiac:parsed-back ast)
			  (zodiac:define-syntaxes-form-names ast)
			  (a-normalize (zodiac:define-syntaxes-form-expr ast) identity)))]
		     
		     ;;---------------------------------------------------------------
		     ;; APPLICATIONS
		     ;;    We will always apply the a-normalization to the function
		     ;;    position of arguments
		     ;;    first normalize the function, then the list of arguments
		     ;;
		     ;; (norm (M A ...) k) ->
		     ;;  (name M 
		     ;;       (lambda fun (name* A .. (lambda term .. (fun term ..)))))
		     [(zodiac:app? ast)
		      (normalize-name 
		       (zodiac:app-fun ast)
		       (lambda (norm-fun)
			 (normalize-name*
			  (zodiac:app-args ast)
			  (lambda (norm-terms)
			    (k (zodiac:make-app (zodiac:zodiac-stx ast)
						(zodiac:parsed-back ast)
						norm-fun
						norm-terms))))))]
		     
		     ;;-----------------------------------------------------------
		     ;; WITH-CONTINUATION-MARK
		     ;;
		     [(zodiac:with-continuation-mark-form? ast)
		      (normalize-name
		       (zodiac:with-continuation-mark-form-key ast)
		       (lambda (key)
			 (normalize-name
			  (zodiac:with-continuation-mark-form-val ast)
			  (lambda (val)
			    (let* ([tname (gensym)]
				   [tbound (zodiac:make-lexical-binding
					    (zodiac:zodiac-stx ast)
					    (make-empty-box)
					    tname
					    tname)]
				   [wcm (zodiac:make-with-continuation-mark-form
					 (zodiac:zodiac-stx ast)
					 (zodiac:parsed-back ast)
					 key val
					 (a-normalize
					  (zodiac:with-continuation-mark-form-body ast)
					  identity))])
			      (set-annotation! wcm tbound)
			      (k wcm))))))]

		     [else (error 'a-normalize "unsupported ~a" ast)]))])
	  a-normalize)))))
