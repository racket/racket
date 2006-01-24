(module ast mzscheme

  (require (lib "contract.ss")
           (planet "hierarchy.ss" ("dherman" "struct.plt" 2 1))
           (planet "inspector.ss" ("dherman" "inspector.plt" 1 0))
           (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           )

  (with-public-inspector
   (define-hierarchy/provide/contract

     (ast ; parent structure for AST nodes
       ([syntax (optional/c syntax?)] ; all nodes store syntax locations
        )
      
       (ast:type () ; parent structure for types
                
         (ast:type:top ()) ; "void" or "unit" type
         (ast:type:bot ()) ; "error" type, or for non-returning operations

         (ast:type:object () ; Object type
           (ast:type:object:any ()) ; Distinguished Any type, top of hierarchy
           (ast:type:object:null ()) ; Type of null, bottom of hierarchy
           (ast:type:object:iface ; Interface type
             ([name identifier?] ; name of the interface
              )))

         (ast:type:primitive ; Builtin type (int, char, bool, etc.)
           ([name symbol?] ; builtins come from a fixed set of names
            ))

         (ast:type:tuple ; Tuple type
           ([elems (listof ast:type?)] ; types of each tuple element
            ))

         (ast:type:partial/tuple ; Tuple type as inferred from selector
           ([position integer?] ; position at which type is known
            [elem ast:type?] ; type at that position
            ))

         (ast:type:function ; Function type (without dispatch)
           ([input ast:type?] ; input type
            [output ast:type?] ; output type
            ))

         (ast:type:method ; Method type (with receiver dispatch)
           ([receiver ast:type:object?] ; type of the method's receiver
            [input ast:type?] ; input type
            [output ast:type?] ; input type
            ))
         )

       (ast:defn () ; parent structure for top-level definitions

         (ast:defn:iface ; Interface definitions
           ([name identifier?] ; interface name
            [supers (listof ast:type:object?)] ; parent interfaces
            [members (listof ast:iface/member?)] ; members (methods and fields)
            ))

         (ast:defn:class ; Class definitions
           ([name identifier?] ; interface name
            [self-type ast:type:object?] ; class's type internally and for subclassing
            [final? boolean?] ; whether the class may be extended
            [client-types (listof ast:type:object?)] ; implemented interfaces
            [formals (listof ast:formal?)] ; constructor arguments
            [members (listof ast:class/member?)] ; member definitions
            [exports (listof ast:export?)] ; export declarations
            ))

         (ast:defn:mixin ; Mixin definition
           ([name identifier?] ; mixin name
            [self-type ast:type:object?] ; mixin's type internally and for subclassing
            [super-type ast:type:object?] ; input interface
            [final? boolean?] ; whether the mixin can be extended
            [client-types (listof ast:type:object?)] ; implemented interfaces
            [formals (listof ast:formal?)] ; constructor arguments
            [super-formals (listof ast:formal?)] ; expected parent arguments
            [super-new ast:super-new?] ; parent initialization
            [pre-members (listof ast:class/member?)] ; members defined before super-new
            [post-members (listof ast:class/member?)] ; members defined after super-new
            [exports (listof ast:export?)] ; export declarations
            ))

         (ast:defn:subclass ; Subclass definition (applies a mixin to a class)
           ([name identifier?] ; new class's name
            [base identifier?] ; superclass
            [mixin identifier?] ; applied mixin
            ))

         (ast:defn:structure ; Structure definition (class and type defined at once)
           ([name identifier?] ; structure name
            [self-type ast:type:object?] ; internal/subclassing type (how does this relate to "structure" type?)
            [final? boolean?] ; whether the structure can be extended
            [client-types (listof ast:type:object?)] ; implemented interfaces
            [formals (listof ast:formal?)] ; constructor arguments
            [members (listof ast:class/member?)] ; member definitions
            [exports (listof ast:export?)] ; export declarations
            ))

         (ast:defn:substructure ; Substructure definition (defines and instantiates subclassing)
           ([name identifier?] ; substructure name
            [self-type ast:type:object?] ; internal/subclassing type
            [super-class identifier?] ; parent class
            [super-type ast:type:object?] ; parent interface
            [final? boolean?] ; whether the substructure can be extended
            [client-types (listof ast:type:object?)] ; implemented interfaces
            [formals (listof ast:formal?)] ; constructor arguments
            [super-formals (listof ast:formal?)] ; expected parent arguments
            [super-new ast:super-new?] ; parent initialization
            [pre-members (listof ast:class/member?)] ; members defined before super-new
            [post-members (listof ast:class/member?)] ; members defined after super-new
            [exports (listof ast:export?)] ; export declarations
            ))

         (ast:defn:function ; Function definition
           ([name identifier?] ; function name
            [return-type ast:type?] ; output type
            [formals (listof ast:formal?)] ; input names and types
            [body ast:expr?] ; function implementation
            ))

         (ast:defn:binding ; Top-level variable binding
           ([names (listof identifier?)] ; variable name or names (if binding a tuple)
            [types (listof ast:type?)] ; variable type(s)
            [init ast:expr?] ; expression providing the bound value(s)
            ))
       
         )

       (ast:iface/member ; Member declared in an interface
         ([name identifier?] ; member name
          )
         (ast:iface/member:field ; Field of an interface
           ([type ast:type?] ; field type
            ))
         (ast:iface/member:method ; Method of an interface
           ([return-type ast:type?] ; output type
            [formal-types (listof ast:type?)] ; input types
            )))

       (ast:class/member ; Member defined in a class/mixin
         ([name identifier?] ; member name
          )
         (ast:class/member:field ; Field of a class
           ([type ast:type?] ; field type
            [default (optional/c ast:expr?)] ; default value
            ))
         (ast:class/member:field/formal ; Field and constructor argument
           ([type ast:type?] ; field type
            [default (optional/c ast:expr?)] ; default value
            ))
         (ast:class/member:method ; Method of a class
           ([return-type ast:type?] ; output type
            [formals (listof ast:formal?)] ; method arguments
            [body ast:expr?] ; method implementation
            )))

       (ast:super-new ; Parent class initializer
         ([args (listof ast:named/arg?)] ; constructor arguments
          ))

       (ast:expr () ; Parent structure for expressions

         (ast:expr:self ()) ; Self-reference within an object
         
         (ast:expr:var ; Variable reference
           ([name identifier?] ; variable name
            ))

         (ast:expr:assign ; Assignment
           ([lhs ast:expr?] ; assignment destination
            [rhs ast:expr?] ; assignment source
            ))

         (ast:expr:apply ; Function call
           ([func ast:expr?] ; invoked function
            [arg ast:expr?] ; actual arguments
            ))

         (ast:expr:literal ; Primitive value
           ([type ast:type?] ; literal type
            [value syntax?] ; literal value
            ))

         (ast:expr:unary/op ; Prefix operation
          (
           [name symbol?] ; operator name
           [rator-stx syntax?] ; operator syntax
           [rator-type (optional/c ast:type?)] ; operator type (presumably added by typechecker)
           [arg ast:expr?] ; operator argument
           ))
         
         (ast:expr:binary/op ; Infix operation
          (
           [name symbol?] ; operator name
           [rator-stx syntax?] ; operator syntax
           [rator-type (optional/c ast:type?)] ; operator type (presumably added by typechecker)
           [left ast:expr?] ; first argument
           [right ast:expr?] ; second argument
           ))

         (ast:expr:function ; Anonymous function value
           ([return-type ast:type?] ; output type
            [formals (listof ast:formal?)] ; arguments
            [body ast:expr?] ; function implementation
            ))

         (ast:expr:if ; Simple conditional
           ([test ast:expr?] ; Boolean expression
            [then ast:expr?] ; Result when true
            [else ast:expr?] ; Result when false
            ))

         (ast:expr:cast ; Typecast
           ([object ast:expr?] ; value to cast
            [type ast:type:object?] ; type of cast
            ))

         (ast:expr:isa ; Type conditional
           ([object ast:expr?] ; value to test
            [type ast:type:object?] ; type of test
            ))

         (ast:expr:member ; Access field or method
           ([object (union ast:expr? (symbols 'my))] ; receiver
            [object-type (optional/c ast:type:object?)] ; receiver type
            [name identifier?] ; member name
            [method? (union boolean? (symbols 'unknown))] ; whether member is a method or field
            ))

         (ast:expr:let ; Local bindings
           ([bindings (listof ast:defn:binding?)] ; new definitions
            [body ast:expr?] ; expression in new scope
            ))

         (ast:expr:sequence ; Sequential statements
           ([statements (listof ast:expr?)] ; executed in order for effect only
            [result ast:expr?] ; executed last and returned
            ))

         (ast:expr:new ; Object construction
           ([class identifier?] ; class to instantiate
            [type (optional/c ast:type:object?)] ; static type of object (presumably added by typechecker)
            [args (listof ast:named/arg?)] ; constructor arguments
            ))

         (ast:expr:cond ; Multiple-branch conditional
           ([clauses (listof ast:cond/clause?)] ; conditional clauses
            [else ast:expr?] ; result if all else fails
            ))

         (ast:expr:while ; Imperative recursion
           ([test ast:expr?] ; controls loop
            [body ast:expr?] ; loop body
            ))

         (ast:expr:return ; Escapes function/method
           ([result ast:expr?] ; returned value
            ))

         (ast:expr:tuple ; Tuple constructor
           ([elems (listof ast:expr?)] ; tuple elements
            ))

         (ast:expr:tuple/select ; Tuple projection
           ([position integer?] ; selected element
            [arg ast:expr?] ; tuple expression
            )))

       (ast:export ; Export declaration
         ([type ast:type:object?] ; exported type
          [members (listof ast:export/member?)] ; exported members
          ))

       (ast:formal ; Declared arguments
         ([name identifier?] ; variable name
          [type ast:type?] ; argument type
          ))

       (ast:cond/clause ; Conditional branches
         ([test ast:expr?] ; condition
          [result ast:expr?] ; result if true
          ))

       (ast:export/member ; Individual export specification
        ([internal identifier?] ; name inside class/mixin
         [external identifier?] ; name from interface
         ))
       
       (ast:named/arg ; By-name arguments
        ([name identifier?] ; argument name
         [actual ast:expr?] ; argument value
         ))

       )))
     
  )
