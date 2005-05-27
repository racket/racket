;; vehicle choosing phase for closures
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

; Assign closures to ``vehicles'', and collect information for
;  MzScheme about the closures.
; A vehicle is a C function that implements the body of a
;  closure. Multiple closures may be assigned to a single
;  vehicle to improve the performance of tail calls.

; The relate-lambdas! procedure is used to put procedure
;  code into equivalence sets. If A contains a tail-call to
;  B, they're put in the same equivalence class, and then
;  they'll be implemented in the same vehicle, so A's call
;  to B can be implemented as a goto.

;;; Annotatitons: ----------------------------------------------
;;    lambda: `code' structure UPDATED: label and
;;       vehicle are set
;;; ------------------------------------------------------------

(module vehicle mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide vehicle@)
  (define vehicle@
    (unit/sig
	compiler:vehicle^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:const^
	      compiler:known^
	      compiler:closure^
	      compiler:driver^)

      ;; Used for union-find for lambda vehicles:
      (define (get-vehicle-top code)
	(let loop ([code code])
	  (let ([c (closure-code-vehicle code)])
	    (if (code? c)
		(let ([top (loop c)])
		  (set-closure-code-vehicle! code top)
		  top)
		code))))
      
      (define-struct vehicle (total-labels lambdas max-arity))
      (define-struct (procedure-vehicle vehicle) (max-args))
      
      (define vehicle:procedure 'vehicle:procedure)

      (define vehicles:automatic 'vehicles:automatic)
      (define vehicles:functions 'vehicles:functions)
      (define vehicles:monolithic 'vehicles:monolithic)
      
      (define (make-empty-vehicle type)
	(case type
	  [(vehicle:procedure) (make-procedure-vehicle 0 null 0 0)]))
      (define (vehicle-is-type? v type)
	(case type
	  [(vehicle:procedure) (procedure-vehicle? v)]))
      
      (define compiler:vehicles #f)
      (define compiler:total-vehicles 0)
      (define vehicle:add-lambda! 
	(lambda (v type l)
	  (let ([old-v (hash-table-get compiler:vehicles v 
				       (lambda () (make-empty-vehicle type)))])
	    (unless (vehicle-is-type? old-v type)
	      (compiler:internal-error
	       #f
	       "can't use vehicle ~a as type ~a"
	       old-v type))
	    (set-vehicle-lambdas! old-v (cons l (vehicle-lambdas old-v)))
	    (hash-table-put! compiler:vehicles v old-v))))
      (define vehicle:register-max-arity!
	(lambda (v n)
	  (set-vehicle-max-arity! v (max n (vehicle-max-arity v)))))
      (define vehicle:register-max-args!
	(lambda (v n)
	  (set-procedure-vehicle-max-args! v (max n (procedure-vehicle-max-args v)))))

      ;; These lists are built up backwards, so reverse it before outputting the list
      (define compiler:case-lambdas null)

      (define (compiler:get-vehicles) compiler:vehicles)
      (define (compiler:get-total-vehicles) compiler:total-vehicles)
      (define (compiler:get-case-lambdas) compiler:case-lambdas)
      
      (define (compiler:init-vehicles!)
	(set! compiler:vehicles (make-hash-table))
	(set! compiler:total-vehicles 0)
	(set! compiler:case-lambdas null))

      (define choose-vehicles!
	(lambda ()
	  (when (eq? (compiler:option:vehicles) vehicles:monolithic) 
	    (set! compiler:total-vehicles (compiler:option:vehicles:monoliths)))

	  (for-each (lambda (L)
		      (let* ([code (get-annotation L)]
			     [type (cond
				    [(zodiac:case-lambda-form? L) vehicle:procedure])]
			     [new-vehicle
			      (lambda ()
				(begin0 compiler:total-vehicles
					(set! compiler:total-vehicles
					      (+ 1 compiler:total-vehicles))))]
			     [vnum (case (compiler:option:vehicles)
				     [(vehicles:automatic)
				      (case type
					[(vehicle:procedure) 
					 (let* ([top (get-vehicle-top code)]
						[n (or (closure-code-vehicle top)
						       (new-vehicle))])
					   (set-closure-code-vehicle! top n)
					   (set-closure-code-vehicle! code n)
					   n)])]
				     [(vehicles:monolithic) 
				      (case type
					[(vehicle:procedure) (random (compiler:option:vehicles:monoliths))])]
				     [(vehicles:functions) (new-vehicle)]
				     [else (compiler:internal-error 
					    #f 
					    (format "bad option:vehicles - ~a" (compiler:option:vehicles)))])])
			(set-closure-code-vehicle! code vnum)
			(vehicle:add-lambda! vnum type L)
			;; assign label, too
			(let* ([vehicle (hash-table-get compiler:vehicles 
							vnum
							(lambda ()
							  (compiler:internal-error 
							   #f "bad hash table lookup (2)~n")))]
			       [curr-label (vehicle-total-labels vehicle)])
			  (vehicle:register-max-arity! vehicle (closure-code-max-arity code))
			  (s:register-max-arity! (closure-code-max-arity code))
			  (cond
			   [(procedure-vehicle? vehicle)
			    (vehicle:register-max-args! 
			     vehicle
			     (apply max
				    (cons
				     0
				     (map (lambda (a) (length (zodiac:arglist-vars a)))
					  (zodiac:case-lambda-form-args L)))))]
			   [else (void)])
			  (set-closure-code-label! code curr-label)
			  (set-vehicle-total-labels! vehicle (+ 1 curr-label)))

			;; We take this opportunity to collect other top-level info
			;; that is closure-type-specific
			(cond
			 [(zodiac:case-lambda-form? L)
			  (unless (= 1 (length (zodiac:case-lambda-form-args L)))
			    (set-procedure-code-case-arities! code (length compiler:case-lambdas))
			    (set! compiler:case-lambdas (cons L compiler:case-lambdas)))])))

		    (compiler:get-closure-list))))

      (define (get-vehicle vehicle-number)
	(hash-table-get compiler:vehicles 
			vehicle-number
			(lambda () 
			  ;; not an error because random placement
			  ;; may leave some vehicles empty
			  (let ([v (make-empty-vehicle vehicle:procedure)])
			    (hash-table-put! compiler:vehicles vehicle-number v)
			    v))))


      ;; Traverse an AST and relate closure current-lambda to Y if
      ;; the AST includes a tail-call to Y.
      (define relate-lambdas!
	(letrec
	    ([same-vehicle!
	      (lambda (a b)
		(let ([a-top (get-vehicle-top (get-annotation a))]
		      [b-top (get-vehicle-top (get-annotation b))])
		  (unless (eq? a-top b-top)
		    (set-closure-code-vehicle! a-top b-top))))]
	     
	     [relate!
	      (lambda (current-lambda ast)
		(cond
		 
		 ;;------------------------------------------------------------------
		 ;; LET EXPRESSIONS
		 ;;
		 [(zodiac:let-values-form? ast)
		  (relate! current-lambda (zodiac:let-values-form-body ast))]

		 [(zodiac:letrec-values-form? ast)
		  (relate! current-lambda (zodiac:letrec-values-form-body ast))]
		 
		 ;;-----------------------------------------------------------------
		 ;; IF EXPRESSIONS
		 ;;
		 [(zodiac:if-form? ast)
		  (relate! current-lambda (zodiac:if-form-then ast))
		  (relate! current-lambda (zodiac:if-form-else ast))]

		 ;;------------------------------------------------------------------
		 ;; BEGIN EXPRESSIONS
		 ;;
		 [(zodiac:begin-form? ast)
		  (let loop ([l (zodiac:begin-form-bodies ast)])
		    (if (null? (cdr l))
			(relate! current-lambda (car l))
			(loop (cdr l))))]
		 
		 ;;------------------------------------------------------------------
		 ;; WITH-CONTINUATION-MARK EXPRESSIONS
		 ;;
		 [(zodiac:with-continuation-mark-form? ast)
		  (relate! current-lambda (zodiac:with-continuation-mark-form-body ast))]
		 
		 ;;-----------------------------------------------------------
		 ;; MODULE
		 ;;
		 [(zodiac:module-form? ast)
		  (relate! current-lambda (zodiac:module-form-body ast))]
		 
		 ;;-----------------------------------------------------------------
		 ;; APPLICATIONS
		 ;;
		 ;; Check for known func & relate to this one
		 ;;
		 [(zodiac:app? ast)
		  (let ([f (zodiac:app-fun ast)])
		    (cond
		     [(or (zodiac:bound-varref? f)
			  (top-level-varref/bind-from-lift? f))
		      (let ([known (extract-varref-known-val f)])
			(and known
			     (when (zodiac:case-lambda-form? known)
			       (same-vehicle! current-lambda known))))]
		     [else (void)]))]
		 
		 [else (void)]))])
	  (lambda (current-lambda ast) (relate! current-lambda ast))))

      (define (vehicle:only-code-in-vehicle? code)
	(= (vehicle-total-labels (get-vehicle (closure-code-vehicle code))) 1)))))

