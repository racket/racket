;; VM-Scheme
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2001 PLT

;; Mostly structure definitions for VM AST nodes.

(module vmscheme mzscheme
  
  (require (lib "unitsig.ss")
	   (lib "list.ss")
	   (lib "etc.ss"))

  (require (lib "zodiac-sig.ss" "syntax"))

  (require "sig.ss")
  (require "../sig.ss")

  (provide vmscheme@)
  (define vmscheme@
    (unit/sig compiler:vmstructs^
      (import compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:zlayer^
	      compiler:driver^)

      ;; Block statements
      (define-struct (vm:sequence zodiac:zodiac) (vals))
      (define-struct (vm:if zodiac:zodiac) (test then else))
      (define-struct (vm:module-body zodiac:zodiac) (vals invoke syntax?))

      ;; Tail position statements
      (define-struct (vm:void zodiac:zodiac) (val))
      (define-struct (vm:return zodiac:zodiac) (val))
      (define-struct (vm:tail-apply zodiac:zodiac) (closure argc prim))
      (define-struct (vm:tail-call zodiac:zodiac) (label closure set-env?))
      (define-struct (vm:continue zodiac:zodiac) ())

      ;; non-tail imperative statements
      (define-struct (vm:set! zodiac:zodiac) (vars val mode))
      (define-struct (vm:generic-args zodiac:zodiac) (closure tail? prim vals))
      (define-struct (vm:register-args zodiac:zodiac) (vars vals))
      (define-struct (vm:args zodiac:zodiac) (type vals))
      (define-struct (vm:begin0-mark! zodiac:zodiac) (var val))
      (define-struct (vm:begin0-setup! zodiac:zodiac) (var))
      (define-struct (vm:syntax! zodiac:zodiac) (vars val in-mod?))

      ;; r-values (1 step computations)
      (define-struct (vm:alloc zodiac:zodiac) (type))
      (define-struct (vm:build-constant zodiac:zodiac) (text))
      (define-struct (vm:make-closure zodiac:zodiac) (closure))
      (define-struct (vm:make-procedure-closure vm:make-closure)
	(vehicle min-arity max-arity name empty? method?))
      (define-struct (vm:make-case-procedure-closure vm:make-closure)
	(vehicle num-cases case-arities name empty? method?))
      (define-struct (vm:apply zodiac:zodiac) 
	(closure argc known? multi? prim simple-tail-prim?))
      (define-struct (vm:macro-apply zodiac:zodiac) 
	(name primitive args tail? bool?))
      (define-struct (vm:call zodiac:zodiac) (label closure))
      (define-struct (vm:begin0-extract zodiac:zodiac) (var))
      (define-struct (vm:wcm-mark! zodiac:zodiac) (key val))
      (define-struct (vm:wcm-push! zodiac:zodiac) (var))
      (define-struct (vm:wcm-pop! zodiac:zodiac) (var))
      (define-struct (vm:wcm-remember! zodiac:zodiac) (var val))
      (define-struct (vm:wcm-extract zodiac:zodiac) (var))
      (define-struct (vm:check-global zodiac:zodiac) (var))
      (define-struct (vm:module-create zodiac:zodiac) (shape id))

      ;; a-values
      (define-struct (vm:global-varref zodiac:zodiac) (var))
      (define-struct (vm:bucket zodiac:zodiac) (var))
      (define-struct (vm:per-load-statics-table zodiac:zodiac) ())
      (define-struct (vm:per-invoke-statics-table zodiac:zodiac) ())
      (define-struct (vm:cast zodiac:zodiac) (val rep)) ; last resort

      ;; l-values (locations in memory)
      (define-struct (vm:local-varref zodiac:zodiac) (var binding))
      (define-struct (vm:static-varref zodiac:zodiac) (var))
      (define-struct (vm:static-varref-from-lift vm:static-varref) (lambda))
      (define-struct (vm:per-load-static-varref vm:static-varref) ())
      (define-struct (vm:per-invoke-static-varref vm:static-varref) ())
      (define-struct (vm:per-load-static-varref-from-lift vm:per-load-static-varref) (lambda))
      (define-struct (vm:per-invoke-static-varref-from-lift vm:per-invoke-static-varref) (lambda))
      (define-struct (vm:primitive-varref zodiac:zodiac) (var))
      (define-struct (vm:symbol-varref zodiac:zodiac) (var))
      (define-struct (vm:inexact-varref zodiac:zodiac) (var))
      (define-struct (vm:struct-ref zodiac:zodiac) (field var))
      (define-struct (vm:deref zodiac:zodiac) (var))
      (define-struct (vm:ref zodiac:zodiac) (var))

      ;; immediate values
      (define-struct (vm:immediate zodiac:zodiac) (text))

      ;; defines a structure type
      ;; all structures may be indexed by number as well.
      (define-struct vm:struct-type (fields))

      (define vm:box-struct-type (make-vm:struct-type '(box)))

      ;; argument types
      (define arg-type:register 'arg-type:register)
      (define arg-type:arg 'arg-type:arg)
      (define arg-type:tail-arg 'arg-type:tail-arg)

      ;; set!-target types
      (define target-type:global 'target-type:global)
      (define target-type:lexical 'target-type:lexical)
      (define target-type:static target-type:lexical)

      ;; this is the class of statements that make control leave the block
      (define vm:control-return?
	(one-of vm:return? vm:tail-apply? vm:tail-call? vm:continue?))

      ;; Defines fixnumness in the VM Scheme.  
      ;; may change under different implementations; and will certainly
      ;; need to change as things get unwrapped...
      (define vm:fixnum?
	(lambda (n)
	  (and (exact? n) (integer? n) (< n (expt 2 30)) (> n (- (expt 2 30))))))

      ;; This function defines whether a constant must be built or not.
      ;; This functions answers #t to constants that may just appear 
      ;; as constants in VM Scheme.
      (define vm:literal-constant?
	(let ([p? (one-of (all-of number? vm:fixnum?)
			  null?
			  boolean?
			  char?
			  void?
			  undefined?)])
	  (lambda (i)
	    (p? (syntax-e (zodiac:zodiac-stx i)))))))))

#|

(define vm:vm->sexp
  (lambda (ast)
    (cond
     [(vm:sequence? ast) `(sequence ,@(map vm:vm->sexp (vm:sequence-vals ast)))]
     [(vm:if? ast) `(if ,(vm:vm->sexp (vm:if-test ast))
			,(vm:vm->sexp (vm:if-then ast))
			,(vm:vm->sexp (vm:if-else ast)))]
     [(vm:void? ast) `(void ,(vm:vm->sexp (vm:void-val ast)))]
     [(vm:return? ast) `(return ,(vm:vm->sexp (vm:return-val ast)))]
     [(vm:tail-apply? ast) `(tail-apply ,(vm:vm->sexp (vm:tail-apply-closure ast))
					(argc ,(vm:tail-apply-argc ast))
					(prim ,(vm:tail-apply-prim ast)))]
     [(vm:tail-call? ast) `(tail-call ,label (set-env? ,(vm:tail-call-set-env? ast)))]
     [(vm:continue? ast) '(continue)]
     [(vm:set!? ast) `(set! ,@(map (lambda (v)
				     `(,(car v) ,(vm:vm->sexp (cdr v))))
				   (vm:set!-vars ast))
			    ,(vm:vm->sexp (vm:set!-val ast)))]
     [(vm:generic-args? ast) 
      `(generic-args (tail? ,(vm:generic-args-tail? ast))
		     (prim ,(vm:generic-args-prim ast))
		     (vals ,@(map vm:vm->sexp (vm:generic-args-vals ast))))]
     [(vm:args? ast)
      `(args (type ,(vm:args-type ast)) (vals ,@(map vm:vm->sexp (vm:args-vals ast))))]
     [(vm:struct? ast)
      (let ([super (vm:struct-super ast)])
	`(struct ,(vm:struct-type ast)
		 ,(if super (vm:vm->sexp (vm:struct-super ast)) #f)
		 ,(vm:struct-fields ast)))]
     [(vm:alloc? ast)
      `(alloc ,(rep->sexp (vm:alloc-type ast)))]
     
     [(vm:build-constant? ast)
      `(build-constant ,(zodiac:zread-object (vm:build-constant-text ast)))]
     [(vm:make-procedure-closure? ast)
      `(make-procedure-closure ,(vm:vm->sexp (vm:make-closure-closure ast))
			       ,(vm:make-closure-min-arity ast)
			       ,(vm:make-closure-max-arity ast))]
     [(vm:apply? ast)
      `(apply ,(vm:vm->sexp (vm:apply-closure ast))
	      (argc ,(vm:apply-argc ast))
	      (known? ,(vm:apply-known? ast))
	      (multi? ,(vm:apply-multi? ast))
	      (prim ,(vm:apply-prim ast)))]
     [(vm:call? ast)
      `(call ,(vm:call-label ast))]
     [(vm:global-varref? ast)
      `(global-varref ,(vm:global-varref-var ast))]
     [(vm:local-varref? ast)
      `(local-varref ,(vm:local-varref-var ast))]
     [(vm:static-varref? ast)
      `(static-varref ,(vm:static-varref-var ast))]
     [(vm:primitive-varref? ast) 
      `(primitive-varref ,(vm:primitive-varref-var ast))]
     [(vm:struct-ref? ast)
      `(struct-ref ,(vm:struct-ref-field ast)
		   ,(vm:vm->sexp (vm:struct-ref-var ast)))]
     [(vm:deref? ast)
      `(deref ,(vm:vm->sexp (vm:deref-var ast)))]
     [(vm:ref? ast)   
      `(ref ,(vm:vm->sexp (vm:ref-var ast)))] 
     [(vm:immediate? ast)
      (let ([text (vm:immediate-text ast)])
	`(immediate ,(cond [(number? text)
			    `(label ,text)]
			   [(zodiac:zread? text)
			    (zodiac:zread-object text)]
			   [else (error 'vm:vm->sexp "~a bad immediate text" text)])))]
     [else
      (error 'vm:vm->sexp "~a not supported" ast)])))
|#
