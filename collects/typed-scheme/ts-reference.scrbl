#lang scribble/doc

@begin[(require scribble/manual scribble/eval
                scheme/sandbox)
       (require (for-label typed-scheme
                           scheme/list srfi/14
                           version/check))]

@begin[
(define (item* header . args) (apply item @bold[header]{: } args))
(define-syntax-rule (tmod forms ...) (schememod typed-scheme forms ...))
(define (gtech . x)  (apply tech x #:doc '(lib "scribblings/guide/guide.scrbl")))
(define (rtech . x)  (apply tech x #:doc '(lib "scribblings/reference/reference.scrbl")))
]

@title[#:tag "top"]{The Typed Scheme Reference} 

@author["Sam Tobin-Hochstadt"]

@(defmodulelang typed-scheme)

@section[#:tag "type-ref"]{Type Reference}

@subsubsub*section{Base Types}
@deftogether[(
@defidform[Number]
@defidform[Integer]
@defidform[Boolean]
@defidform[String]
@defidform[Keyword]
@defidform[Symbol]
@defidform[Void]
@defidform[Input-Port]
@defidform[Output-Port]
@defidform[Path]
@defidform[Regexp]
@defidform[PRegexp]
@defidform[Syntax]
@defidform[Identifier]
@defidform[Bytes]
@defidform[Namespace]
@defidform[EOF]
@defidform[Continuation-Mark-Set]
@defidform[Char])]{
These types represent primitive Scheme data.  Note that @scheme[Integer] represents exact integers.}

@defidform[Any]{Any Scheme value. All other types are subtypes of @scheme[Any].}

@defidform[Nothing]{The type with no members.}

The following base types are parameteric in their type arguments.

@defform[(Listof t)]{Homogenous @rtech{lists} of @scheme[t]}
@defform[(Boxof t)]{A @rtech{box} of @scheme[t]}
@defform[(Syntaxof t)]{A @rtech{syntax object} containing a @scheme[t]}
@defform[(Vectorof t)]{Homogenous @rtech{vectors} of @scheme[t]}
@defform[(Option t)]{Either @scheme[t] of @scheme[#f]}
@defform*[[(Parameter t)
           (Parameter s t)]]{A @rtech{parameter} of @scheme[t].  If two type arguments are supplied, 
                               the first is the type the parameter accepts, and the second is the type returned.}
@defform[(Pair s t)]{is the pair containing @scheme[s] as the @scheme[car]
  and @scheme[t] as the @scheme[cdr]}
@defform[(HashTable k v)]{is the type of a @rtech{hash table} with key type
   @scheme[k] and value type @scheme[v].}

@subsubsub*section{Type Constructors}

@defform*[#:id -> #:literals (* ...)
	       [(dom ... -> rng)
	        (dom ... rest * -> rng)
		(dom ... rest ... bound -> rng)
                (dom -> rng : pred)]]{is the type of functions from the (possibly-empty)
  sequence @scheme[dom ...] to the @scheme[rng] type.  The second form
  specifies a uniform rest argument of type @scheme[rest], and the
  third form specifies a non-uniform rest argument of type
  @scheme[rest] with bound @scheme[bound].  In the third form, the
  second occurrence of @scheme[...] is literal, and @scheme[bound]
  must be an identifier denoting a type variable. In the fourth form, 
  there must be only one @scheme[dom] and @scheme[pred] is the type 
  checked by the predicate.}
@defform[(U t ...)]{is the union of the types @scheme[t ...]}
@defform[(case-lambda fun-ty ...)]{is a function that behaves like all of
  the @scheme[fun-ty]s.  The @scheme[fun-ty]s must all be function
  types constructed with @scheme[->].}
@defform/none[(t t1 t2 ...)]{is the instantiation of the parametric type
  @scheme[t] at types @scheme[t1 t2 ...]}
@defform[(All (v ...) t)]{is a parameterization of type @scheme[t], with
  type variables @scheme[v ...]}
@defform[(List t ...)]{is the type of the list with one element, in order, 
  for each type provided to the @scheme[List] type constructor.}
@defform[(values t ...)]{is the type of a sequence of multiple values, with
types @scheme[t ...].  This can only appear as the return type of a
function.}
@defform/none[v]{where @scheme[v] is a number, boolean or string, is the singleton type containing only that value}
@defform/none[(quote val)]{where @scheme[val] is a Scheme value, is the singleton type containing only that value}
@defform/none[i]{where @scheme[i] is an identifier can be a reference to a type
name or a type variable}
@defform[(Rec n t)]{is a recursive type where @scheme[n] is bound to the
recursive type in the body @scheme[t]}

Other types cannot be written by the programmer, but are used
internally and may appear in error messages.

@defform/none[(struct:n (t ...))]{is the type of structures named
@scheme[n] with field types @scheme[t].  There may be multiple such
types with the same printed representation.}
@defform/none[<n>]{is the printed representation of a reference to the
type variable @scheme[n]}

@section[#:tag "special-forms"]{Special Form Reference}

Typed Scheme provides a variety of special forms above and beyond
those in PLT Scheme.  They are used for annotating variables with types,
creating new types, and annotating expressions.

@subsection{Binding Forms}

@scheme[_loop], @scheme[_f], @scheme[_a], and @scheme[_v] are names, @scheme[_t] is a type.
 @scheme[_e] is an expression and @scheme[_body] is a block.

@defform*[[
  (let: ([v : t e] ...) . body)
  (let: loop : t0 ([v : t e] ...) . body)]]{
Local bindings, like @scheme[let], each with
associated types.  In the second form, @scheme[_t0] is the type of the
result of @scheme[_loop] (and thus the result of the entire
			      expression as well as the final
				expression in @scheme[body]).}
@deftogether[[
@defform[(letrec: ([v : t e] ...) . body)]
@defform[(let*: ([v : t e] ...) . body)]]]{Type-annotated versions of
@scheme[letrec] and @scheme[let*].}

@deftogether[[
@defform[(let/cc: v : t . body)]
@defform[(let/ec: v : t . body)]]]{Type-annotated versions of
@scheme[let/cc] and @scheme[let/ec].  @scheme[t] is the type that will be provided to the continuation @scheme[v].}

@subsection{Anonymous Functions}

@defform/subs[(lambda: formals . body)
([formals ([v : t] ...) 
	  ([v : t] ... . [v : t])])]{
A function of the formal arguments @scheme[v], where each formal
argument has the associated type.  If a rest argument is present, then
it has type @scheme[(Listof t)].}
@defform[(λ: formals . body)]{
An alias for the same form using @scheme[lambda:].}
@defform[(plambda: (a ...) formals . body)]{
A polymorphic function, abstracted over the type variables
@scheme[a]. The type variables @scheme[a] are bound in both the types
of the formal, and in any type expressions in the @scheme[body].}
@defform[(case-lambda: [formals body] ...)]{
A function of multiple arities.  Note that each @scheme[formals] must have a
different arity.}
@defform[(pcase-lambda: (a ...) [formals body] ...)]{
A polymorphic function of multiple arities.}

@subsection{Loops}

@defform/subs[(do: : u ([id : t init-expr step-expr-maybe] ...)
                       (stop?-expr finish-expr ...)
                expr ...+)
              ([step-expr-maybe code:blank
                                step-expr])]{
Like @scheme[do], but each @scheme[id] having the associated type @scheme[t], and 
the final body @scheme[expr] having the type @scheme[u].
}


@subsection{Definitions}

@defform*[[(define: v : t e)
	   (define: (f . formals) : t . body)	   
	   (define: (a ...) (f . formals) : t . body)]]{
These forms define variables, with annotated types.  The first form
defines @scheme[v] with type @scheme[t] and value @scheme[e].  The
second and third forms defines a function @scheme[f] with appropriate
types.  In most cases, use of @scheme[:] is preferred to use of @scheme[define:].}



@subsection{Structure Definitions}
@defform/subs[
(define-struct: maybe-type-vars name-spec ([f : t] ...))
([maybe-type-vars code:blank (v ...)]
 [name-spec name (name parent)])]{
 Defines a @rtech{structure} with the name @scheme[name], where the
 fields @scheme[f] have types @scheme[t].  When @scheme[parent], the
structure is a substructure of @scheme[parent].  When
@scheme[maybe-type-vars] is present, the structure is polymorphic in the type
 variables @scheme[v].}
                                 
@defform/subs[
(define-struct/exec: name-spec ([f : t] ...) [e : proc-t])
([name-spec name (name parent)])]{
 Like @scheme[define-struct:], but defines an procedural structure.  
 The procdure @scheme[e] is used as the value for @scheme[prop:procedure], and must have type @scheme[proc-t].}

@subsection{Type Aliases}
@defform*[[(define-type-alias name t)
	   (define-type-alias (name v ...) t)]]{
The first form defines @scheme[name] as type, with the same meaning as
@scheme[t].  The second form is equivalent to
@scheme[(define-type-alias name (All (v ...) t))].  Type aliases may
refer to other type aliases or types defined in the same module, but
cycles among type aliases are prohibited.}


@subsection{Type Annotation and Instantiation}

@defform[(: v t)]{This declares that @scheme[v] has type @scheme[t].
The definition of @scheme[v] must appear after this declaration.  This
can be used anywhere a definition form may be used.}

@defform[(provide: [v t] ...)]{This declares that the @scheme[v]s have
the types @scheme[t], and also provides all of the @scheme[v]s.}

@litchar{#{v : t}} This declares that the variable @scheme[v] has type
@scheme[t].  This is legal only for binding occurences of @scheme[_v].

@defform[(ann e t)]{Ensure that @scheme[e] has type @scheme[t], or
some subtype.  The entire expression has type @scheme[t].
This is legal only in expression contexts.}

@litchar{#{e :: t}} This is identical to @scheme[(ann e t)].

@defform[(inst e t ...)]{Instantiate the type of @scheme[e] with types
@scheme[t ...].  @scheme[e] must have a polymorphic type with the
appropriate number of type variables. This is legal only in expression
contexts.}

@litchar|{#{e @ t ...}}| This is identical to @scheme[(inst e t ...)].

@subsection{Require}

Here, @scheme[_m] is a module spec, @scheme[_pred] is an identifier
naming a predicate, and @scheme[_r] is an optionally-renamed identifier.

@defform/subs[#:literals (struct opaque)
(require/typed m rt-clause ...)
([rt-clause [r t]
	    [struct name ([f : t] ...)]
	    [struct (name parent) ([f : t] ...)]
	    [opaque t pred]])
]{This form requires identifiers from the module @scheme[m], giving
them the specified types.   

The first form requires @scheme[r], giving it type @scheme[t].

@index["struct"]{The second and third forms} require the struct with name @scheme[name]
with fields @scheme[f ...], where each field has type @scheme[t].  The
third form allows a @scheme[parent] structure type to be specified.
The parent type must already be a structure type known to Typed
Scheme, either built-in or via @scheme[require/typed].  The
structure predicate has the appropriate Typed Scheme filter type so
that it may be used as a predicate in @scheme[if] expressions in Typed
Scheme.

@index["opaque"]{The fourth case} defines a new type @scheme[t].  @scheme[pred], imported from
module @scheme[m], is a predicate for this type.  The type is defined
as precisely those values to which @scheme[pred] produces
@scheme[#t].  @scheme[pred] must have type @scheme[(Any -> Boolean)].  
Opaque types must be required lexically before they are used.

In all cases, the identifiers are protected with @rtech{contracts} which
enforce the specified types.  If this contract fails, the module
@scheme[m] is blamed. 

Some types, notably polymorphic types constructed with @scheme[All],
cannot be converted to contracts and raise a static error when used in
a @scheme[require/typed] form.}

@section{Libraries Provided With Typed Scheme}

The @schememodname[typed-scheme] language corresponds to the
@schememodname[scheme/base] language---that is, any identifier provided
by @schememodname[scheme/base], such as @scheme[modulo] is available by default in
@schememodname[typed-scheme].  

@schememod[typed-scheme
(modulo 12 2)
]

Any value provided by @schememodname[scheme] is available by simply
@scheme[require]ing it; use of @scheme[require/typed] is not
neccessary.  

@schememod[typed-scheme
(require scheme/list)
(display (first (list 1 2 3)))
]

Some libraries have counterparts in the @schemeidfont{typed}
collection, which provide the same exports as the untyped versions.
Such libraries include @schememodname[srfi/14],
@schememodname[net/url], and many others.  

@schememod[typed-scheme
(require typed/srfi/14)
(char-set= (string->char-set "hello")
           (string->char-set "olleh"))
]

To participate in making more libraries available, please visit 
@link["http://www.ccs.neu.edu/home/samth/adapt/"]{here}.


Other libraries can be used with Typed Scheme via
@scheme[require/typed].

@schememod[typed-scheme
(require/typed version/check
               [check-version (-> (U Symbol (Listof Any)))])
(check-version)
]

@section{Typed Scheme Syntax Without Type Checking}

@defmodulelang[typed-scheme/no-check]

On occasions where the Typed Scheme syntax is useful, but actual
typechecking is not desired, the @schememodname[typed-scheme/no-check]
language is useful.  It provides the same bindings and syntax as Typed
Scheme, but does no type checking.

Examples:

@schememod[typed-scheme/no-check
(: x Number)
(define x "not-a-number")]
