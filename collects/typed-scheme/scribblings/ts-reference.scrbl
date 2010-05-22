#lang scribble/manual

@begin[(require "utils.rkt" scribble/eval
                racket/sandbox)
       (require (for-label (only-meta-in 0 typed/racket)
                           racket/list srfi/14
                           version/check))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))

@title[#:tag "top"]{The Typed Racket Reference} 

@author["Sam Tobin-Hochstadt"]

@(defmodulelang* (typed/racket/base typed/racket)
                 #:use-sources (typed-scheme/typed-scheme typed-scheme/private/prims))

@section[#:tag "type-ref"]{Type Reference}

@subsubsub*section{Base Types}
@deftogether[(
@defidform[Number]
@defidform[Complex]
@defidform[Real]
@defidform[Integer]
@defidform[Natural]
@defidform[Exact-Positive-Integer]
@defidform[Exact-Nonnegative-Integer]
@defidform[Boolean]
@defidform[True]
@defidform[False]
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
These types represent primitive Racket data.  Note that @racket[Integer] represents exact integers.}

@defidform[Any]{Any Racket value. All other types are subtypes of @racket[Any].}

@defidform[Nothing]{The empty type.  No values inhabit this type, and
any expression of this type will not evaluate to a value.}


The following base types are parameteric in their type arguments.

@defform[(Listof t)]{Homogenous @rtech{lists} of @racket[t]}
@defform[(Boxof t)]{A @rtech{box} of @racket[t]}
@defform[(Syntaxof t)]{A @rtech{syntax object} containing a @racket[t]}
@defform[(Vectorof t)]{Homogenous @rtech{vectors} of @racket[t]}
@defform[(Option t)]{Either @racket[t] of @racket[#f]}
@defform*[[(Parameter t)
           (Parameter s t)]]{A @rtech{parameter} of @racket[t].  If two type arguments are supplied, 
                               the first is the type the parameter accepts, and the second is the type returned.}
@defform[(Pair s t)]{is the pair containing @racket[s] as the @racket[car]
  and @racket[t] as the @racket[cdr]}
@defform[(HashTable k v)]{is the type of a @rtech{hash table} with key type
   @racket[k] and value type @racket[v].}

@subsubsub*section{Type Constructors}

@defform*[#:id -> #:literals (* ...)
	       [(dom ... -> rng)
	        (dom ... rest * -> rng)
		(dom ... rest ... bound -> rng)
                (dom -> rng : pred)]]{is the type of functions from the (possibly-empty)
  sequence @racket[dom ...] to the @racket[rng] type.  The second form
  specifies a uniform rest argument of type @racket[rest], and the
  third form specifies a non-uniform rest argument of type
  @racket[rest] with bound @racket[bound].  In the third form, the
  second occurrence of @racket[...] is literal, and @racket[bound]
  must be an identifier denoting a type variable. In the fourth form, 
  there must be only one @racket[dom] and @racket[pred] is the type 
  checked by the predicate.}
@defform[(U t ...)]{is the union of the types @racket[t ...]}
@defform[(case-lambda fun-ty ...)]{is a function that behaves like all of
  the @racket[fun-ty]s.  The @racket[fun-ty]s must all be function
  types constructed with @racket[->].}
@defform/none[(t t1 t2 ...)]{is the instantiation of the parametric type
  @racket[t] at types @racket[t1 t2 ...]}
@defform[(All (v ...) t)]{is a parameterization of type @racket[t], with
  type variables @racket[v ...]}
@defform[(List t ...)]{is the type of the list with one element, in order, 
  for each type provided to the @racket[List] type constructor.}
@defform[(Vector t ...)]{is the type of the list with one element, in order, 
  for each type provided to the @racket[Vector] type constructor.}
@defform[(values t ...)]{is the type of a sequence of multiple values, with
types @racket[t ...].  This can only appear as the return type of a
function.}
@defform/none[v]{where @racket[v] is a number, boolean or string, is the singleton type containing only that value}
@defform/none[(quote val)]{where @racket[val] is a Racket value, is the singleton type containing only that value}
@defform/none[i]{where @racket[i] is an identifier can be a reference to a type
name or a type variable}
@defform[(Rec n t)]{is a recursive type where @racket[n] is bound to the
recursive type in the body @racket[t]}

Other types cannot be written by the programmer, but are used
internally and may appear in error messages.

@defform/none[(struct:n (t ...))]{is the type of structures named
@racket[n] with field types @racket[t].  There may be multiple such
types with the same printed representation.}
@defform/none[<n>]{is the printed representation of a reference to the
type variable @racket[n]}

@section[#:tag "special-forms"]{Special Form Reference}

Typed Racket provides a variety of special forms above and beyond
those in Racket.  They are used for annotating variables with types,
creating new types, and annotating expressions.

@subsection{Binding Forms}

@racket[_loop], @racket[_f], @racket[_a], and @racket[_v] are names, @racket[_t] is a type.
 @racket[_e] is an expression and @racket[_body] is a block.

@defform*[[
  (let: ([v : t e] ...) . body)
  (let: loop : t0 ([v : t e] ...) . body)]]{
Local bindings, like @racket[let], each with
associated types.  In the second form, @racket[_t0] is the type of the
result of @racket[_loop] (and thus the result of the entire
			      expression as well as the final
				expression in @racket[body]).}
@deftogether[[
@defform[(letrec: ([v : t e] ...) . body)]
@defform[(let*: ([v : t e] ...) . body)]
@defform[(let-values: ([([v : t] ...) e] ...) . body)]
@defform[(letrec-values: ([([v : t] ...) e] ...) . body)]
@defform[(let*-values: ([([v : t] ...) e] ...) . body)]]]{
Type-annotated versions of
@racket[letrec], @racket[let*], @racket[let-values],
  @racket[letrec-values], and @racket[let*-values].}  

@deftogether[[
@defform[(let/cc: v : t . body)]
@defform[(let/ec: v : t . body)]]]{Type-annotated versions of
@racket[let/cc] and @racket[let/ec].}

@subsection{Anonymous Functions}

@defform/subs[(lambda: formals . body)
([formals ([v : t] ...) 
	  ([v : t] ... . [v : t])])]{
A function of the formal arguments @racket[v], where each formal
argument has the associated type.  If a rest argument is present, then
it has type @racket[(Listof t)].}
@defform[(λ: formals . body)]{
An alias for the same form using @racket[lambda:].}
@defform[(plambda: (a ...) formals . body)]{
A polymorphic function, abstracted over the type variables
@racket[a]. The type variables @racket[a] are bound in both the types
of the formal, and in any type expressions in the @racket[body].}
@defform[(case-lambda: [formals body] ...)]{
A function of multiple arities.  Note that each @racket[formals] must have a
different arity.}
@defform[(pcase-lambda: (a ...) [formals body] ...)]{
A polymorphic function of multiple arities.}

@subsection{Loops}

@defform/subs[(do: : u ([id : t init-expr step-expr-maybe] ...)
                       (stop?-expr finish-expr ...)
                expr ...+)
              ([step-expr-maybe code:blank
                                step-expr])]{
Like @racket[do], but each @racket[id] having the associated type @racket[t], and 
the final body @racket[expr] having the type @racket[u].
}


@subsection{Definitions}

@defform*[[(define: v : t e)
	   (define: (f . formals) : t . body)	   
	   (define: (a ...) (f . formals) : t . body)]]{
These forms define variables, with annotated types.  The first form
defines @racket[v] with type @racket[t] and value @racket[e].  The
second and third forms defines a function @racket[f] with appropriate
types.  In most cases, use of @racket[:] is preferred to use of @racket[define:].}



@subsection{Structure Definitions}
@defform/subs[
(define-struct: maybe-type-vars name-spec ([f : t] ...))
([maybe-type-vars code:blank (v ...)]
 [name-spec name (name parent)])]{
 Defines a @rtech{structure} with the name @racket[name], where the
 fields @racket[f] have types @racket[t].  When @racket[parent], the
structure is a substructure of @racket[parent].  When
@racket[maybe-type-vars] is present, the structure is polymorphic in the type
 variables @racket[v].}
                                 
@defform/subs[
(define-struct/exec: name-spec ([f : t] ...) [e : proc-t])
([name-spec name (name parent)])]{
 Like @racket[define-struct:], but defines an procedural structure.  
 The procdure @racket[e] is used as the value for @racket[prop:procedure], and must have type @racket[proc-t].}

@subsection{Names for Types}
@defform*[[(define-type name t)
	   (define-type (name v ...) t)]]{
The first form defines @racket[name] as type, with the same meaning as
@racket[t].  The second form is equivalent to
@racket[(define-type name (All (v ...) t))].  Type names may
refer to other types defined in the same module, but
cycles among them are prohibited.}

@subsection{Generating Predicates Automatically}
@defform[(define-predicate name t)]{
Defines @racket[name] as a predicate for the type @racket[t].
@racket[name] has the type @racket[(Any -> Boolean : t)]. 
@racket[t] may not contain function types.}


@subsection{Type Annotation and Instantiation}

@defform[(: v t)]{This declares that @racket[v] has type @racket[t].
The definition of @racket[v] must appear after this declaration.  This
can be used anywhere a definition form may be used.}

@defform[(provide: [v t] ...)]{This declares that the @racket[v]s have
the types @racket[t], and also provides all of the @racket[v]s.}

@litchar{#{v : t}} This declares that the variable @racket[v] has type
@racket[t].  This is legal only for binding occurences of @racket[_v].

@defform[(ann e t)]{Ensure that @racket[e] has type @racket[t], or
some subtype.  The entire expression has type @racket[t].
This is legal only in expression contexts.}

@litchar{#{e :: t}} This is identical to @racket[(ann e t)].

@defform[(inst e t ...)]{Instantiate the type of @racket[e] with types
@racket[t ...].  @racket[e] must have a polymorphic type with the
appropriate number of type variables. This is legal only in expression
contexts.}

@litchar|{#{e @ t ...}}| This is identical to @racket[(inst e t ...)].

@subsection{Require}

Here, @racket[_m] is a module spec, @racket[_pred] is an identifier
naming a predicate, and @racket[_r] is an optionally-renamed identifier.

@defform/subs[#:literals (struct opaque)
(require/typed m rt-clause ...)
([rt-clause [r t]
	    [struct name ([f : t] ...)]
	    [struct (name parent) ([f : t] ...)]
	    [opaque t pred]])
]{This form requires identifiers from the module @racket[m], giving
them the specified types.   

The first form requires @racket[r], giving it type @racket[t].

@index["struct"]{The second and third forms} require the struct with name @racket[name]
with fields @racket[f ...], where each field has type @racket[t].  The
third form allows a @racket[parent] structure type to be specified.
The parent type must already be a structure type known to Typed
Racket, either built-in or via @racket[require/typed].  The
structure predicate has the appropriate Typed Racket filter type so
that it may be used as a predicate in @racket[if] expressions in Typed
Racket.

@index["opaque"]{The fourth case} defines a new type @racket[t].  @racket[pred], imported from
module @racket[m], is a predicate for this type.  The type is defined
as precisely those values to which @racket[pred] produces
@racket[#t].  @racket[pred] must have type @racket[(Any -> Boolean)].  
Opaque types must be required lexically before they are used.

In all cases, the identifiers are protected with @rtech{contracts} which
enforce the specified types.  If this contract fails, the module
@racket[m] is blamed. 

Some types, notably polymorphic types constructed with @racket[All],
cannot be converted to contracts and raise a static error when used in
a @racket[require/typed] form.}

@section{Libraries Provided With Typed Racket}

The @racketmodname[typed/racket] language corresponds to the
@racketmodname[racket] language---that is, any identifier provided
by @racketmodname[racket], such as @racket[modulo] is available by default in
@racketmodname[typed/racket].  

@racketmod[typed/racket
(modulo 12 2)
]

The @racketmodname[typed/racket/base] language corresponds to the
@racketmodname[racket/base] language.

Some libraries have counterparts in the @racketidfont{typed}
collection, which provide the same exports as the untyped versions.
Such libraries include @racketmodname[srfi/14],
@racketmodname[net/url], and many others.  

@racketmod[typed/racket
(require typed/srfi/14)
(char-set= (string->char-set "hello")
           (string->char-set "olleh"))
]

To participate in making more libraries available, please visit 
@link["http://www.ccs.neu.edu/home/samth/adapt/"]{here}.


Other libraries can be used with Typed Racket via
@racket[require/typed].

@racketmod[typed/racket
(require/typed version/check
               [check-version (-> (U Symbol (Listof Any)))])
(check-version)
]

@section{Typed Racket Syntax Without Type Checking}

@defmodulelang[typed-scheme/no-check]

On occasions where the Typed Racket syntax is useful, but actual
typechecking is not desired, the @racketmodname[typed-scheme/no-check]
language is useful.  It provides the same bindings and syntax as Typed
Racket, but does no type checking.

Examples:

@racketmod[typed-scheme/no-check
(: x Number)
(define x "not-a-number")]

@section{Typed Regions}

The @racket[with-type] for allows for localized Typed Racket regions in otherwise untyped code.

@defform*/subs[[(with-type result-spec fv-clause body ...+)
                (with-type export-spec fv-clause body ...+)]
              ([fv-clause code:blank
                          (code:line #:freevars ([id fv-type] ...))]
               [result-spec (code:line #:result type)]
               [export-spec ([export-id export-type] ...)])]{
The first form, an expression, checks that @racket[body ...+] has the type @racket[type].
If the last expression in @racket[body ...+] returns multiple values, @racket[type] must
be a type of the form @racket[(values t ...)].
Uses of the result values are appropriately checked by contracts generated from 
@racket[type].

The second form, which can be used as a definition, checks that each of the @racket[export-id]s 
has the specified type.  These types are also enforced in the surrounding code with contracts.

The @racket[id]s are assumed to 
have the types ascribed to them; these types are converted to contracts and checked dynamically.

@examples[#:eval the-eval
(with-type #:result Number 3)

((with-type #:result (Number -> Number)
   (lambda: ([x : Number]) (add1 x)))
 #f)

(let ([x "hello"])
  (with-type #:result String
    #:freevars ([x String])
    (string-append x ", world")))

(let ([x 'hello])
  (with-type #:result String
    #:freevars ([x String])
    (string-append x ", world")))

(with-type ([fun (Number -> Number)]
            [val Number])
  (define (fun x) x)
  (define val 17))

(fun val)]
}
