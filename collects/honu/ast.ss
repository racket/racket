(module ast mzscheme

  (require (lib "contract.ss")
           "contract.ss"
           "utils.ss")
  
  (define-struct/p/c ast ; parent of AST hierarchy
    ([syntax (maybe syntax?)] ; all nodes have syntax information
     ))
  
  ;; Type AST nodes
  (define-struct/p/c (ast:type ast) ()) ; parent of type hierarchy
  (define-struct/p/c (ast:type:top ast:type) ()) ; "void" or "unit" type
  (define-struct/p/c (ast:type:bot ast:type) ()) ; "bottom" type, used for errors, nontermination, etc.
  (define-struct/p/c (ast:type:prim ast:type); primitive types (int, string, char, etc.)
    ([name symbol?] ; name of the primitive type
     ))
  (define-struct/p/c (ast:type:tuple ast:type) ; tuple types
    ([elems (listof ast:type?)] ; types of each tuple position
     ))
  (define-struct/p/c (ast:type:partial-tuple ast:type) ; partial tuple type information
    ([position integer?] ; which position of the tuple is known
     [type ast:type?] ; the type of the known tuple position
     ))
  (define-struct/p/c (ast:type:func ast:type) ; function types (non-dispatching)
    ([arg ast:type?] ; argument type
     [ret ast:type?] ; return type
     ))
  (define-struct/p/c (ast:type:method ast:type) ; method types (dispatching functions)
    ([dispatch ast:type?] ; the static type on which to dispatch
     [arg ast:type?] ; argument type
     [ret ast:type?] ; return type
     ))
  (define-struct/p/c (ast:type:iface ast:type) ; standard named interfaces
    ([name identifier?] ; interface name
     ))
  (define-struct/p/c (ast:type:iface-top ast:type) ()) ; special Any interface
  (define-struct/p/c (ast:type:iface-bot ast:type) ()) ; special type for null

  ;; Definition AST nodes
  (define-struct/p (ast:defn ast) ()) ; used for defn?
  (define-struct/p (ast:defn:iface ast:defn) (name supers members)) ; used for interface definitions
  (define-struct/p (ast:defn:class ast:defn) (name type final? impls inits             ; used for class definitions
                                                 members exports))
  (define-struct/p (ast:defn:mixin ast:defn) (name type sub-type final? impls          ; used for mixin definitions 
                                                 inits withs super-new
                                                 members-before members-after exports))       
  (define-struct/p (ast:defn:subclass ast:defn) (name base mixin)) ; used for subclass definitions

  (define-struct/p (ast:defn:struct ast:defn) (name type final? impls inits             ; used for structs, later replaced with components
                                                  members exports))
  (define-struct/p (ast:defn:substruct ast:defn) (name type base arg-type final? impls     ; same, but for structs that are subclasses
                                                     inits withs super-new
                                                     members-before members-after exports))

  (define-struct/p (ast:defn:function ast:defn) (name type formals body)) ; used for function definitions
  (define-struct/p (ast:defn:bind ast:defn) (names types value)) ; used for top-level definitions
  
  ;; AST nodes for member declarations (in interfaces)
  (define-struct/p (ast:member-decl ast) (name)) ; member-decl?
  (define-struct/p (ast:member-decl:field ast:member-decl) (type)) ; used for field declarations
  (define-struct/p (ast:member-decl:method ast:member-decl) (type arg-types)) ; used for method declarations
  
  ;; AST nodes for member definitions (in classes/mixins)
  (define-struct/p (ast:member-defn ast) (name)) ; member-defn?
  (define-struct/p (ast:member-defn:init-field ast:member-defn) (type value)) ; used for init fields (value can be #f or expression AST)
  (define-struct/p (ast:member-defn:field ast:member-defn) (type value)) ; used for fields (value can be #f or expression AST)
  (define-struct/p (ast:member-defn:method ast:member-defn) (type formals body)) ; used for methods
  
  ;; AST node for super call (just in mixins/subclasses)
  (define-struct/p (ast:super-new ast) (args))
  
  ;; Expression AST nodes
  (define-struct/p (ast:expr ast) ())
  (define-struct/p (ast:expr:this ast:expr) ())
  (define-struct/p (ast:expr:var ast:expr) (name))
  (define-struct/p (ast:expr:assn ast:expr) (lhs rhs))
  (define-struct/p (ast:expr:call ast:expr) (func arg))
  (define-struct/p (ast:expr:lit ast:expr) (type value))
  (define-struct/p (ast:expr:un-op ast:expr) (op op-stx op-type arg))
  (define-struct/p (ast:expr:bin-op ast:expr) (op op-stx op-type larg rarg))
  (define-struct/p (ast:expr:lambda ast:expr) (type formals body))
  (define-struct/p (ast:expr:if ast:expr) (cond then else))
  (define-struct/p (ast:expr:cast ast:expr) (obj type))
  (define-struct/p (ast:expr:isa ast:expr) (obj type))
  (define-struct/p (ast:expr:member ast:expr) (obj elab name method?)) ;; method is only needed for translation
  (define-struct/p (ast:expr:let ast:expr) (bindings body)) 
  (define-struct/p (ast:expr:seq ast:expr) (effects value))
  (define-struct/p (ast:expr:new ast:expr) (class type args))
  (define-struct/p (ast:expr:cond ast:expr) (clauses else))
  (define-struct/p (ast:expr:while ast:expr) (cond body))
  (define-struct/p (ast:expr:return ast:expr) (body))
  (define-struct/p (ast:expr:tuple ast:expr) (vals))
  (define-struct/p (ast:expr:select ast:expr) (slot arg))
  
  ;; Miscellaneous AST nodes
  (define-struct/p (ast:binding ast) (names types value)) ; used for bindings in lets
  (define-struct/p (ast:export ast) (type binds)) ; used for export statements
  (define-struct/p (ast:export-bind ast) (old new)) ; used for export bindings
  (define-struct/p (ast:formal ast) (name type)) ; used for formal arguments
  (define-struct/p (ast:name-arg ast) (name value)) ; used for by-name arguments (like new)
  (define-struct/p (ast:cond-clause ast) (pred rhs)) ; used for cond clauses
  )
