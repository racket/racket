(module ast mzscheme
  
  (provide (all-defined))
  
  (define-syntax (define-honu-struct stx)
    (syntax-case stx ()
      [(_ (id sup) (field ...))
       (with-syntax [(new-id
                      (datum->syntax-object
                       #'id
                       (string->symbol
                        (string-append "honu:" (symbol->string (syntax-e #'id)))) #'id #'id))
                     (new-sup
                      (datum->syntax-object
                       #'sup
                       (string->symbol
                        (string-append "honu:" (symbol->string (syntax-e #'sup)))) #'sup #'sup))]
         #'(define-struct (new-id new-sup) (field ...) #f))]
      [(_ id (field ...))
       (with-syntax [(new-id (datum->syntax-object #'id (string->symbol (string-append "honu:" (symbol->string (syntax-e #'id)))) #'id #'id))]
         #'(define-struct new-id (field ...) #f))]))
  
  (define-honu-struct ast (stx)) ; ensures that all nodes have a stx obj
  
  ;; Type AST nodes
  (define-honu-struct (type           ast)  ())               ; used only for type?
  (define-honu-struct (type-top       type) ())               ; used to represent "void" (i.e. unit)
  (define-honu-struct (type-bot       type) ())               ; used to represent a term okay in _any_ context (like error)
  (define-honu-struct (type-prim      type) (name))           ; used to represent primitive types (int, string, char, etc.)
  (define-honu-struct (type-tuple     type) (args))           ; used to represent a tuple of types
  (define-honu-struct (type-func      type) (arg ret))        ; used for functions (no dispatch)
  (define-honu-struct (type-disp      type) (disp arg ret))   ; used for methods (single dispatch) or multi-dispatch (if later added)
  (define-honu-struct (type-iface     type) (name))           ; used for interface types (except for next two, which are more specific)
  (define-honu-struct (type-iface-top type) ())               ; used for the Any type
  (define-honu-struct (type-iface-bot type) ())               ; used for the type that null has
  
  (define-honu-struct (type-select    type) (slot type))      ; used only for context type in the typechecker when #n is encountered

  ;; Definition AST nodes
  (define-honu-struct (defn      ast)  ())                                       ; used for defn?
  (define-honu-struct (iface     defn) (name supers members))                    ; used for interface definitions
  (define-honu-struct (class     defn) (name type final? impls inits             ; used for class definitions
                                        members exports))
  (define-honu-struct (mixin     defn) (name type sub-type final? impls          ; used for mixin definitions 
                                        inits withs super-new
                                        members-before members-after exports))       
  (define-honu-struct (subclass  defn) (name base mixin))                        ; used for subclass definitions

  (define-honu-struct (struct    defn) (name type final? impls inits             ; used for structs, later replaced with components
                                        members exports))
  (define-honu-struct (substruct defn) (name type base arg-type final? impls     ; same, but for structs that are subclasses
                                        inits withs super-new
                                        members-before members-after exports))

  (define-honu-struct (function  defn) (name type formals body))                 ; used for function definitions
  (define-honu-struct (bind-top  defn) (names types value))                      ; used for top-level definitions
  
  ;; AST nodes for member declarations (in interfaces)
  (define-honu-struct (member-decl ast)         ())                    ; member-decl?
  (define-honu-struct (field-decl  member-decl) (name type))           ; used for field declarations
  (define-honu-struct (method-decl member-decl) (name type arg-types)) ; used for method declarations
  
  ;; AST nodes for member definitions (in classes/mixins)
  (define-honu-struct (member-defn ast)         ())                       ; member-defn?
  (define-honu-struct (init-field  member-defn) (name type value))        ; used for init fields (value can be #f or expression AST)
  (define-honu-struct (field       member-defn) (name type value))        ; used for fields (value can be #f or expression AST)
  (define-honu-struct (method      member-defn) (name type formals body)) ; used for methods
  
  ;; AST node for super call (just in mixins/subclasses)
  (define-honu-struct (super-new   ast) (args))
  
  ;; Expression AST nodes
  (define-honu-struct (expr   ast)  ())
  (define-honu-struct (this   expr) ())
  (define-honu-struct (var    expr) (name))
  (define-honu-struct (assn   expr) (lhs rhs))
  (define-honu-struct (call   expr) (func arg))
  (define-honu-struct (lit    expr) (type value))
  (define-honu-struct (un-op  expr) (op op-stx op-type arg))
  (define-honu-struct (bin-op expr) (op op-stx op-type larg rarg))
  (define-honu-struct (lambda expr) (type formals body))
  (define-honu-struct (if     expr) (cond then else))
  (define-honu-struct (cast   expr) (obj type))
  (define-honu-struct (isa    expr) (obj type))
  (define-honu-struct (member expr) (obj elab name method?)) ;; method is only needed for translation
  (define-honu-struct (let    expr) (bindings body)) 
  (define-honu-struct (seq    expr) (effects value))
  (define-honu-struct (new    expr) (class type args))
  (define-honu-struct (cond   expr) (clauses else))
  (define-honu-struct (while  expr) (cond body))
  (define-honu-struct (return expr) (body))
  (define-honu-struct (tuple  expr) (vals))
  (define-honu-struct (select expr) (slot arg))
  
  ;; Miscellaneous AST nodes
  (define-honu-struct (binding     ast) (names types value)) ; used for bindings in lets
  (define-honu-struct (export      ast) (type binds))        ; used for export statements
  (define-honu-struct exp-bind          (old new))           ; used for export bindings
  (define-honu-struct (formal      ast) (name type))         ; used for formal arguments
  (define-honu-struct name-arg          (name value))        ; used for by-name arguments (like new)
  (define-honu-struct (cond-clause ast) (pred rhs))          ; used for cond clauses
  )
