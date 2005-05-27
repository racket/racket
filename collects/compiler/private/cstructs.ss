;; Compiler structures
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

;; Mostly structure definitions, mostly for annotations.

(module cstructs mzscheme
  (require (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide cstructs@)
  (define cstructs@
    (unit/sig compiler:cstructs^
      (import compiler:library^
	      (zodiac : zodiac^)
	      compiler:zlayer^)

      ;;----------------------------------------------------------------------------
      ;; VARREF ATTRIBUTES
      ;;  Used as the annotation for zodiac:varref objects

      (define-struct va (flags invoke-module))

      (define (varref:empty-attributes) (make-va 0 #f))
      (define (varref:add-attribute! ast attr)
	(let ([va (get-annotation ast)])
	  (let ([attr (if (varref:module-invoke? attr)
			  (begin
			    (set-va-invoke-module! va attr)
			    varref:per-invoke-static)
			  attr)])
	    (set-va-flags! va (bitwise-ior attr (va-flags va))))))
      (define (varref:has-attribute? ast attr)
	(let ([anno (get-annotation ast)])
	  (and (va? anno) (positive? (bitwise-and (va-flags anno) attr)))))
      (define (varref:invoke-module ast)
	(let ([anno (get-annotation ast)])
	  (and (va? anno) (va-invoke-module anno))))

      (define varref:static 1)
      (define varref:per-load-static 2)
      (define varref:per-invoke-static 4)
      (define varref:primitive 8)
      (define varref:symbol 16)
      (define varref:inexact 32)
      (define varref:env 64)
      (define varref:in-module 128)
      (define varref:module-stx-string 256)

      (define mi-counter -1)
      (define-struct varref:module-invoke (id syntax? context-path-index))
      (define (make-module-invokes self-path-index)
	(set! mi-counter (add1 mi-counter))
	(values (make-varref:module-invoke mi-counter #f self-path-index)
		(make-varref:module-invoke mi-counter #t self-path-index)))

      (define (get-num-module-invokes)
	(add1 mi-counter))

      (define (is-module-invoke? mi num)
	(and (varref:module-invoke? mi)
	     (= num (varref:module-invoke-id mi))))

      (define (varref:reset-module-id!) (set! mi-counter -1))
      
      ;;----------------------------------------------------------------------------
      ;; AST NODES
      ;;  New AST nodes to augment the zodiac set:
      
      ;; AST node for the creation of a closure (replaces, e.g., a lambda expression)
      (define-struct (compiler:make-closure zodiac:zodiac) (lambda free-vars args name))

      ;;----------------------------------------------------------------------------
      ;; ANNOTATION STRUCTURES
      ;;

      ;; mzc annotation for a zodiac:binding, installed in the `known'
      ;; analysis phase
      (define-struct binding (rec?       ; part of a letrec recursive binding set
			      mutable?   ; set!ed? (but not for unit or letrec definitions)
			      unit-i/e?  ; is imported/exported (including uses by invoke)
			      anchor     ; zodiac:binding - anchor binding for this binding
			      letrec-set?; set! to implement a letrec
			      ivar?      ; is a class ivar?
			      known?     ; known to have a fixed value? (i.e., it's not
			      ;; mutated or detectably #<undefined> for a while?)
			      val
			      ;; ``known'' value as an abitrary AST (so it's
			      ;; really only *known* if this is a constant
			      known-but-used? 
			      ;; known value used in an improper way?
			      ;; if so, always preserve the variable (i.e., don't
			      ;; propagate it away entirely)
			      rep        ; reprsentation (#f until rep-choosing phase)
			      ))

      ;; copy a binding record
      (define (copy-binding b)
	(make-binding (binding-rec? b)
		      (binding-mutable? b)
		      (binding-unit-i/e? b)
		      (binding-anchor b)
		      (binding-letrec-set? b)
		      (binding-ivar? b)
		      (binding-known? b)
		      (binding-val b)
		      (binding-known-but-used? b)
		      (binding-rep b)))

      (define (copy-binding-for-light-closures b)
	(make-binding #f
		      #f
		      #f
		      #f
		      #f 
		      #f
		      (binding-known? b) (binding-val b)
		      #f
		      #f))

      (define binder:empty-anno
	(make-binding #f
		      #f
		      #f
		      #f
		      #f
		      #f
		      #f
		      #f
		      #f
		      #f))

      (define-struct code (; The following fields, XXX-vars, are
			   ;; all sets of zodiac:bindings
			   free-vars
			   ;; lexical variables that are free in the 
			   ;; code (i.e., kept in a closure)
			   local-vars 
			   ;; variables introduced during the evaluation
			   ;; of the code; includes, for example, the argument
			   ;; variables if this is a lambda closure
			   global-vars 
			   ;; ``global'' variables used by this code;
			   ;; we capture globals that are specific to
			   ;; the namespace at load-time
			   used-vars
			   ;; local variables that are eventually used in
			   ;; an expression after they are introduced in the
			   ;; code
			   captured-vars 
			   ;; free and used variables that are free within
			   ;; a closure that is created by this code

			   parent
			   ;; #f if this is a top-level expression, container
			   ;; code otherwise
			   case-parent
			   ;; #f, unless it's a code in a case-lambda, then
			   ;; it's the case-code containing this code

			   children
			   ;; list of children code structures
			   ))

      ;; Structure for the annotation given to closures, such
      ;;  as lambdas or units. The actual annotation will be
      ;;  an instance of a sub-type of `code', depending on
      ;;  the kind of closure.
      (define-struct (closure-code code)
	(; Representation and implementation info
	 rep 
	 alloc-rep
	 label   ; integer - id within vehicle
	 vehicle ; integer - vehicle id
	 
	 max-arity
	 ;; max number of args in applications
	 ;; within the closure (which is unrelated
	 ;; to the number of arguments used to invoke
	 ;; this closure, if it happens to be a
	 ;; lambda)
	 
	 return-multi 
	 ;; #f (always single), #t (never single), 
	 ;; or 'possible
	 
	 name
	 ;; inferred name - can be #f, a varref, a binding,
	 ;;                 or a list of inferred names.
	 ;;   (see also vm->c:extract-inferred-name)
	 ))

      ;; Annotation type for case-lambda closures:
      (define-struct (procedure-code closure-code) 
	(case-codes 
	 ;; A list of case-code records
	 case-arities
	 ;; An integer indicating which
	 ;; arity record in compiler:case-lambdas
	 ;; contains MzScheme information for
	 ;; the arity of the case-lambda. For
	 ;; single-case lambdas, this is #f
	 ;; because the arity information is
	 ;; inlined.
	 liftable
	 ;; top-level-varref => procedure is lifted
	 method?
	 ;; #t => arity errors hide first argument
	 ;;  (triggered by 'method-arity-error property)
	 ))

      (define-struct (case-code code)
	(; Does the compilation of this case use continue?
	 ;; If so, output the case body within while(1){...}
	 has-continue?))

      ;; annotations given to zodiac:app AST nodes
      (define-struct app (tail?
			  ;; tail application?
			  prim?
			  ;; application of a known primitive?
			  prim-name
			  ;; MzScheme name for the known primitive, or #f
			  ))

      (define-struct module-info (invoke
				  ;; a module-invoke record
				  syntax-invoke
				  ;; another module-invoke record
				  part
				  ;; 'body, 'syntax-body, or 'constructor
				  ))

      (define varref:current-invoke-module (make-parameter #f))

      ;;----------------------------------------------------------------------------
      ;; ACCESSOR
      ;;

      ;; Retrives the *annotation* of a zodiac:binding for a zodiac:bound-varref.
      ;; (Compare to zodiac:bound-varref-binding, which returns the
      ;; zodiac:binding itself, rather than its annotation.)
      (define compiler:bound-varref->binding 
	(compose get-annotation zodiac:bound-varref-binding))

      ;;----------------------------------------------------------------------------
      ;; special constants
      ;;
      (define-struct c-lambda (function-name scheme-name body arity))
      
      ;;----------------------------------------------------------------------------
      ;; error/warning structures
      ;;
      (define-struct compiler:message (ast message))
      (define-struct (compiler:error-msg compiler:message) ())
      (define-struct (compiler:fatal-error-msg compiler:message) ())
      (define-struct (compiler:internal-error-msg compiler:message) ())
      (define-struct (compiler:warning-msg compiler:message) ()))))
