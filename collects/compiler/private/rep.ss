;; Representation choosing phase of the the compiler
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-201 PLT

;; Chooses the representation of all bindings, and also
;;  closures.

;; Currently, all variables for Scheme values are represented
;;  as Scheme_Object* values. But representations are also
;;  chosen for closures and indirected Scheme variables, so
;;  not everything is a Scheme_Object*.

;;; Annotatitons: ----------------------------------------------
;;    binding - `binding' structure UPDATED: rep field set
;;; ------------------------------------------------------------

(module rep mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide rep@)
  (define rep@
    (unit/sig compiler:rep^
      (import compiler:library^
	      compiler:cstructs^
	      compiler:analyze^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:const^
	      compiler:vehicle^
	      compiler:driver^)

      ;;----------------------------------------------------------------------------
      ;; REPRESENTATION (TYPE) LANGUAGE
      ;;
      ;; future : add const?
      ;;

      (define-struct rep:atomic (type))
      ;; Where type is one of: 
      ;;  'scheme-object 
      ;;  'scheme-bucket
      ;;  'scheme-per-load-static
      ;;  'scheme-per-invoke-static
      ;;  'label
      ;;  'prim
      ;;  'prim-case
      ;;  'begin0-saver
      ;;  'wcm-saver
      (define-struct rep:pointer (to))
      (define-struct rep:struct (name orig-name fields))
      (define-struct rep:struct-field (name orig-name rep))

      (define-struct (rep:atomic/invoke rep:atomic) (module-invoke))

      (define (rep:same-shape? a b)
	(let ([al (rep:struct-fields a)]
	      [bl (rep:struct-fields b)])
	  (and (= (length al) (length bl))
	       (andmap (lambda (af bf)
			 (let ([ar (rep:struct-field-rep af)]
			       [br (rep:struct-field-rep bf)]) 
			   (or (and (rep:atomic? ar)
				    (rep:atomic? br)
				    (eq? (rep:atomic-type ar)
					 (rep:atomic-type br))
				    (or (not (rep:atomic/invoke? ar))
					(eq? (rep:atomic/invoke-module-invoke ar)
					     (rep:atomic/invoke-module-invoke br))))
			       (and (rep:struct? ar)
				    (rep:struct? br)
				    (eq? (rep:struct-name ar)
					 (rep:struct-name br))))))
		       al bl))))

      (define compiler:struct-index 0)
      (define compiler:structs null)
      (define (compiler:init-structs!)
	(set! compiler:structs null))
      (define compiler:add-struct!
	(lambda (struct)
	  (let loop ([l compiler:structs])
	    (cond
	     [(null? l)
	      (let ([name (string->symbol (format "mergedStructs~a" compiler:struct-index))])
		(set! compiler:struct-index (add1 compiler:struct-index))
		(set-rep:struct-name! struct name)
		(let loop ([l (rep:struct-fields struct)][n 0])
		  (unless (null? l)
		    (unless (rep:struct-field-name (car l))
		      (set-rep:struct-field-name! (car l)
						  (string->symbol
						   (format "f~a" n))))
		    (loop (cdr l) (add1 n)))))
	      (set! compiler:structs (cons struct compiler:structs))]
	     [(rep:same-shape? struct (car l))
	      (set-rep:struct-name! struct (rep:struct-name (car l)))
	      (let loop ([nl (rep:struct-fields struct)]
			 [ol (rep:struct-fields (car l))])
		(unless (null? nl)
		  (set-rep:struct-field-name! (car nl)
					      (rep:struct-field-name (car ol)))
		  (loop (cdr nl) (cdr ol))))]
	     [else (loop (cdr l))]))))
      (define (compiler:get-structs) compiler:structs)

      (define (rep:find-field struct orig-name)
	(let loop ([l (rep:struct-fields struct)])
	  (if (null? l)
	      (compiler:internal-error 
	       #f 
	       (format
		"vm:find-field: ~a not found in ~a" orig-name
		(rep:struct-fields struct)))
	      (if (eq? (rep:struct-field-orig-name (car l)) orig-name)
		  (rep:struct-field-name (car l))
		  (loop (cdr l))))))

      ;;----------------------------------------------------------------------------
      ;; choose-binding-representations! implements the lion's share of work in 
      ;; chosing representations.  It takes 3 inputs:
      ;;   1) a <set> of variables occurring local to an expression
      ;;   2) a <set> of those variables which are globals
      ;;   3) a <set> of those variables which are used
      ;;   4) a <set> of those variables which are captured
      ;; and returns no values
      ;;
      ;; As a side effect, it sets the representation fields of all those
      ;; struct:bindings living in those compiler:bound guys.

      (define choose-binding-representations!
	(lambda (local-vars global-vars used-vars captured-vars)
	  (let ([set-rep!
		 (lambda (local-var)
		   (let ([binding (get-annotation local-var)])
		     (unless (binding-rep binding)
		       (set-binding-rep! binding
					 (if (or (binding-mutable? binding)
						 (binding-letrec-set? binding)
						 (binding-ivar? binding))
					     (make-rep:pointer 
					      (make-rep:atomic 'scheme-object))
					     (make-rep:atomic 'scheme-object))))))])
	    (for-each set-rep! (set->list local-vars)))))

      ;;----------------------------------------------------------------------------
      ;; choose-closure-representation! chooses representations for a closure
      ;; it takes 1 input
      ;;   1) a code structure
      ;;
      ;; and returns no values
      ;;
      ;; As a side effect, it sets the closure-code-rep field of the code structure
      ;; based on its free variables.  It must be called _after_ binding
      ;; representations have been chosen
      ;;
      (define choose-closure-representation!
	(lambda (code)
	  (let* ([base (gensym)]
		 [struct (let ([fields
				(append! (if (vehicle:only-code-in-vehicle? code)
					     null
					     (list
					      (make-rep:struct-field 'label
								     'label
								     (make-rep:atomic 'label))))
					 (map (lambda (bound)
						(make-rep:struct-field
						 ;; field-name
						 #f
						 (zodiac:binding-var bound)
						 ;; field-type
						 (binding-rep (get-annotation bound))))
					      (set->list (code-free-vars code)))
					 (map (lambda (global)
						(make-rep:struct-field
						 ;; field-name
						 (if (const:per-load-statics-table? global)
						     'pls
						     (if (varref:module-invoke? global)
							 'pmis
							 #f))
						 (if (or (const:per-load-statics-table? global)
							 (varref:module-invoke? global))
						     global
						     (mod-glob-cname global))
						 ;; field-type
						 (if (const:per-load-statics-table? global)
						     (make-rep:atomic 'scheme-per-load-static)
						     (if (varref:module-invoke? global)
							 (make-rep:atomic/invoke 
							  'scheme-per-invoke-static
							  global)
							 (make-rep:atomic 'scheme-bucket)))))
					      (set->list (code-global-vars code))))])
			   (if (null? fields)
			       #f ; empty structure - don't use anything
			       (make-rep:struct 
				;; name
				#f
				(symbol-append 'struct base)
				fields)))])
	    (when struct
	      (compiler:add-struct! struct))
	    (let* ([fields (append (cond
				    [(procedure-code? code)
				     (list
				      (make-rep:struct-field 'prim
							     'prim
							     (if (= 1 (length (procedure-code-case-codes code)))
								 (make-rep:atomic 'prim)
								 (make-rep:atomic 'prim-case))))]
				    [else
				     (compiler:internal-error
				      #f
				      "unknown closure code type: ~s" code)])
				   (if struct
				       (list
					(make-rep:struct-field 'data
							       'data
							       struct))
				       null))]
		   [alloc-struct (if (null? fields)
				     #f
				     (make-rep:struct 
					; name
				      #f
				      (symbol-append 'allocstruct base)
				      fields))])
	      (when alloc-struct
		(compiler:add-struct! alloc-struct))
	      
	      (set-closure-code-rep! code struct)
	      (set-closure-code-alloc-rep! code alloc-struct)))))

      )))
