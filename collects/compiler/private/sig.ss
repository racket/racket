
(module sig mzscheme
  (require (lib "unit.ss"))

  (require "../sig.ss")
  (require (lib "zodiac-sig.ss" "syntax"))

  (define for-syntax-in-env-stx #'for-syntax-in-env)
  (provide for-syntax-in-env-stx)

  (provide compiler:library^)
  (define-signature compiler:library^
    (logical-inverse
     one-of
     all-of
     none-of
     vector-map
     improper-map
     begin-map!
     begin-map
     map!
     list-index
     list-last

     set?
     empty-set
     make-singleton-set
     list->set
     set->list
     improper-list->set
     set-memq?
     set-empty?
     set-find
     set-union
     set-union-singleton
     set-minus
     set-remove
     set-intersect
     set-subset?
     set-map
     set-filter
     remove-duplicates
     symbol-append
     compiler:formals->arity
     compiler:formals->arity*
     compiler:gensym
     compiler:genlabel
     compiler:clean-string
     protect-comment
     global-defined-value*

     compiler:get-label-number
     compiler:reset-label-number!))

  (provide compiler:cstructs^)
  (define-signature compiler:cstructs^
    (varref:empty-attributes
     varref:add-attribute!
     varref:has-attribute?

     varref:static
     varref:per-load-static
     varref:primitive
     varref:symbol
     varref:inexact
     varref:env
     varref:in-module
     varref:module-stx-string

     (struct compiler:make-closure (lambda free-vars args name))
     
     (struct binding (rec?       ; part of a letrec recursive binding set
		      mutable?   ; set!ed?
		      anchor     ; anchor binding for this binding
		      letrec-set?; set! for a letrec definition
		      ivar?      ; is a class ivar?
		      known? val ; has known value?
		      known-but-used? ; known value used in an improper way?
		      rep))      ; reprsentation
     copy-binding

     copy-binding-for-light-closures

     binder:empty-anno 

     (struct code (free-vars local-vars global-vars used-vars captured-vars
			     parent case-parent children))

     (struct closure-code (rep alloc-rep label vehicle
			       max-arity
			       return-multi ; #f, #t, or 'possible
			       name))

     (struct procedure-code (case-codes case-arities liftable method?))

     (struct case-code (has-continue?))

     (struct app (tail? prim? prim-name))

     compiler:bound-varref->binding 

     (struct c-lambda (function-name scheme-name body arity))

     (struct compiler:message (ast message))
     (struct compiler:error-msg ())
     (struct compiler:fatal-error-msg ())
     (struct compiler:internal-error-msg ())
     (struct compiler:warning-msg ())))

  (provide compiler:zlayer^)
  (define-signature compiler:zlayer^
    (static-error
     dynamic-error
     internal-error
     
     compiler:empty-annotation
     make-empty-box
     get-annotation
     set-annotation!
     annotated?
     remove-annotation!

     compiler:escape-on-error

     zodiac:begin0-form-first
     zodiac:begin0-form-rest
     zodiac:set-begin0-form-first!
     zodiac:set-begin0-form-rest!

     undefined?

     zodiac:make-special-constant
     self_modidx
     
     zodiac:binding->lexical-varref

     main-source-file
     zodiac:print-start!

     zodiac->sexp/annotate))

  (provide compiler:prephase^)
  (define-signature compiler:prephase^
    (prephase:init-binding-properties!
     prephase:set-mutable!
     prephase:set-binding-anchor!
     prephase:is-mutable?
     prephase:is-ivar?
     prephase:binding-anchor
     prephase:known-val
     prephase:set-known-val!

     prephase!))

  (provide compiler:anorm^)
  (define-signature compiler:anorm^
    (a-normalize))

  (provide compiler:const^)
  (define-signature compiler:const^
    (const:init-tables!
     const:the-per-load-statics-table
     const:per-load-statics-table?

     const:get-symbol-counter
     const:get-symbol-table

     const:get-inexact-counter
     const:get-inexact-table

     const:get-string-table
     const:get-bytes-table
     const:intern-string

     compiler:add-const!
     compiler:get-symbol-const!
     compiler:construct-const-code!

     compiler:get-static-list
     compiler:get-per-load-static-list
     
     compiler:add-per-load-static-list!

     compiler:make-const-constructor

     (struct compiled-string (id len))))

  (provide compiler:rep^)
  (define-signature compiler:rep^
    ((struct rep:atomic (type))
     (struct rep:pointer (to))
     (struct rep:struct (name orig-name fields))
     (struct rep:struct-field (name orig-name rep))

     compiler:get-structs
     compiler:init-structs!

     rep:find-field

     choose-binding-representations!
     choose-closure-representation!))

  (provide compiler:known^)
  (define-signature compiler:known^
    (make-unknown-letbound-binding
     extract-varref-known-val
     extract-ast-known-value
     analyze-knowns!))

  (provide compiler:analyze^)
  (define-signature compiler:analyze^
    (compiler:get-global-symbols
     compiler:get-primitive-refs

     compiler:get-define-list
     compiler:get-per-load-define-list

     compiler:init-define-lists!

     compiler:add-global-varref!
     compiler:add-primitive-varref!

     compiler:add-local-define-list!
     compiler:add-local-per-load-define-list!
     
     (struct case-info (body case-code global-vars used-vars captured-vars max-arity))

     (struct mod-glob (cname modname varname position exp-time? exp-def? in-module?))
     compiler:get-module-path-constant

     analyze-expression!))

  (provide compiler:lift^)
  (define-signature compiler:lift^
    (lift-lambdas!
     set-single-module-mode!))

  (provide compiler:closure^)
  (define-signature compiler:closure^
    (compiler:get-closure-list

     compiler:get-once-closures-list
     compiler:get-once-closures-globals-list
     compiler:get-lifted-lambdas
     compiler:get-lifted-lambda-vars

     compiler:init-closure-lists!
     compiler:init-once-closure-lists!
     compiler:init-lifted-lambda-list!
     compiler:add-lifted-lambda!
     (struct top-level-varref/bind-from-lift (lambda pls?))

     closure-expression!))

  (provide compiler:vehicle^)
  (define-signature compiler:vehicle^
    ((struct vehicle (total-labels lambdas max-arity))
     (struct procedure-vehicle (max-args))

     compiler:get-vehicles
     compiler:get-total-vehicles
     compiler:get-case-lambdas

     compiler:init-vehicles!

     get-vehicle
     relate-lambdas!

     vehicle:only-code-in-vehicle?

     choose-vehicles!))

  (provide compiler:vmstructs^)
  (define-signature compiler:vmstructs^
    ((struct vm:sequence (vals))
     (struct vm:if (test then else))

     (struct vm:void (val magic?))
     (struct vm:return (val magic?))
     (struct vm:tail-apply (closure argc prim))
     (struct vm:tail-call (label closure set-env?))
     (struct vm:continue ())
     
     (struct vm:set! (vars val mode))
     (struct vm:generic-args (closure tail? magic? prim vals))
     (struct vm:register-args (vars vals))
     (struct vm:args (type vals))
     (struct vm:begin0-mark! (var val))
     (struct vm:begin0-setup! (var))
     (struct vm:syntax! (vars val in-mod?))
     
     (struct vm:global-prepare (vec pos))
     (struct vm:global-lookup (vec pos))
     (struct vm:global-assign (vec val pos))
     (struct vm:safe-vector-ref (vec pos))

     (struct vm:alloc (type))
     (struct vm:build-constant (text))
     (struct vm:make-closure (closure))
     (struct vm:make-procedure-closure (vehicle min-arity max-arity name empty? method?))
     (struct vm:make-case-procedure-closure (vehicle num-cases case-arities name empty? method?))
     (struct vm:apply (closure argc known? multi? prim simple-tail-prim?))
     (struct vm:macro-apply (name primitive args tail? magic? bool?))
     (struct vm:call (label closure))
     (struct vm:begin0-extract (var))
     (struct vm:wcm-mark! (key val))
     (struct vm:wcm-push! (var))
     (struct vm:wcm-pop! (var))
     (struct vm:wcm-remember! (var val))
     (struct vm:wcm-extract (var))
     (struct vm:check-global (var))
     (struct vm:module-create (shape id))

     (struct vm:global-varref (var))
     (struct vm:bucket (var))
     (struct vm:per-load-statics-table ())
     (struct vm:cast (val rep)) ; last resort

     (struct vm:local-varref (var binding))
     (struct vm:static-varref (var))
     (struct vm:static-varref-from-lift (lambda))
     (struct vm:per-load-static-varref ())
     (struct vm:per-load-static-varref-from-lift (lambda))
     (struct vm:primitive-varref (var))
     (struct vm:symbol-varref (var))
     (struct vm:inexact-varref (var))
     (struct vm:struct-ref (field var))
     (struct vm:deref (var))
     (struct vm:ref (var))

     (struct vm:immediate (text))
     
     (struct vm:struct-type (fields))

     vm:box-struct-type

     arg-type:register
     arg-type:arg
     arg-type:tail-arg

     target-type:global
     target-type:lexical
     target-type:static

     vm:control-return?

     vm:fixnum?
     
     vm:literal-constant?))

  (provide compiler:vmphase^)
  (define-signature compiler:vmphase^
    (vm:convert-bound-varref
     vm-phase))

  (provide compiler:vmopt^)
  (define-signature compiler:vmopt^
    (vm-optimize!))

  (provide compiler:driver^)
  (define-signature compiler:driver^ extends compiler:inner^
    (compiler:error
     compiler:fatal-error
     compiler:internal-error
     compiler:warning

     get-s:file-block
     s:register-max-arity!
     compiler:get-setup-suffix

     compiler:multi-o-constant-pool

     register-c-lambda-function
     register-c-declaration

     debug
     debug:get-port))

  (provide compiler:top-level^)
  (define-signature compiler:top-level^
    ((struct block (source codes bytecodes magics max-arity))
     make-empty-block
     block:register-max-arity!

     add-code-local+used-vars!
     remove-code-free-vars!))

  (provide compiler:vm2c^)
  (define-signature compiler:vm2c^
    (vm->c:generate-modglob-name

     vm->c:indent-by
     vm->c:indent-spaces

     vm->c:extract-inferred-name

     vm->c:emit-symbol-list!
     vm->c:emit-symbol-declarations!
     vm->c:emit-symbol-definitions!
     vm->c:emit-bytecode-string-definition!
     vm->c:emit-inexact-declarations!
     vm->c:emit-inexact-definitions!
     vm->c:emit-string-declarations!
     vm->c:emit-prim-ref-declarations!
     vm->c:emit-prim-ref-definitions!
     vm->c:emit-struct-definitions!
     vm->c:emit-static-declarations!
     vm->c:emit-registration!
     vm->c:emit-case-arities-definitions!
     vm->c:emit-top-levels!
     vm->c:emit-vehicle-prototype
     vm->c:emit-vehicle-declaration
     vm->c:emit-vehicle-header
     vm->c:emit-vehicle-prologue
     vm->c:emit-vehicle-epilogue
     vm->c:convert-type-definition
     vm->c:emit-function-prologue
     vm->c:emit-case-prologue
     vm->c:emit-case-epilogue
     vm->c:emit-function-epilogue
     vm->c-expression)))
