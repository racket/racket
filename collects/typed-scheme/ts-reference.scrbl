#lang scribble/doc

@begin[(require scribble/manual)
       (require (for-label typed-scheme))]

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
These types represent primitive Scheme data.
@defidform[Number]{A @gtech{number}}
@defidform[Integer]{An @gtech{integer}}
@defidform[Boolean]{Either @scheme[#t] or @scheme[#f]}
@defidform[String]{A @gtech{string}}
@defidform[Keyword]{A literal @gtech{keyword}}
@defidform[Symbol]{A @gtech{symbol}}
@defidform[Void]{@|void-const|}
@defidform[Port]{A @gtech{port}}
@defidform[Path]{A @rtech{path}}
@defidform[Char]{A @gtech{character}}

@defidform[Any]{Any value}

The following base types are parameteric in their type arguments.

@defform[(Listof t)]{Homogenous @gtech{lists} of @scheme[t]}
@defform[(Boxof t)]{A @gtech{box} of @scheme[t]}
@defform[(Vectorof t)]{Homogenous @gtech{vectors} of @scheme[t]}
@defform[(Option t)]{Either @scheme[t] of @scheme[#f]}
@defform*[[(Parameter t)
           (Parameter s t)]]{A @rtech{parameter} of @scheme[t].  If two type arguments are supplied, 
                               the first is the type the parameter accepts, and the second is the type returned.}
@defform[(Pair s t)]{is the pair containing @scheme[s] as the @scheme[car]
  and @scheme[t] as the @scheme[cdr]}

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
@defform/none['sym]{where @scheme[sym] is a symbol, is the singleton type containing only that symbol}
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

@defform*[[(define: v : t e)
	   (define: (f [v : t] ...) : t . body)	   
	   (define: (a ...) (f [v : t] ...) : t . body)]]{
These forms define variables, with annotated types.  The first form
defines @scheme[v] with type @scheme[t] and value @scheme[e].  The
second and third forms defines a function @scheme[f] with appropriate
types.  In most cases, use of @scheme[:] is preferred to use of @scheme[define:].}

@defform*[[
  (let: ([v : t e] ...) . body)
  (let: loop : t0 ([v : t e] ...) . body)]]{where @scheme[_t0] is the type of the
  result of @scheme[_loop] (and thus the result of the entire expression).}
@defform[
  (letrec: ([v : t e] ...) . body)]{}
@defform[
  (let*: ([v : t e] ...) . body)]{}
@defform*[[
  (lambda: ([v : t] ...) . body)
  (lambda: ([v : t] ... . [v : t]) . body)]]{}
@defform*[[
  (plambda: (a ...) ([v : t] ...) . body)
  (plambda: (a ...) ([v : t] ... . [v : t]) . body)]]{}
@defform[
  (case-lambda: [formals body] ...)]{where @scheme[_formals] is like
  the second element of a @scheme[lambda:]}
@defform[
  (pcase-lambda: (a ...) [formals body] ...)]{where @scheme[_formals] is like
  the second element of a @scheme[lambda:].}


@subsection{Structure Definitions}
@defform*[[
(define-struct: name ([f : t] ...))
(define-struct: (name parent) ([f : t] ...))
(define-struct: (v ...) name ([f : t] ...))
(define-struct: (v ...) (name parent) ([f : t] ...))]]{
 Defines a @rtech{structure} with the name @scheme[name], where the
 fields @scheme[f] have types @scheme[t].  The second and fourth forms
 define @scheme[name] to be a substructure of @scheme[parent].  The
 last two forms define structures that are polymorphic in the type
 variables @scheme[v].}

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

@defform*[[
(require/typed r t m)
(require/typed m [r t] ...)
]]{The first form requires @scheme[r] from module @scheme[m], giving
it type @scheme[t].  The second form generalizes this to multiple identifiers.}

@defform[(require/opaque-type t pred m)]{
This defines a new type @scheme[t].  @scheme[pred], imported from
module @scheme[m], is a predicate for this type.  The type is defined
as precisely those values to which @scheme[pred] produces
@scheme[#t].  @scheme[pred] must have type @scheme[(Any -> Boolean)].}

@defform[(require-typed-struct name ([f : t] ...) m)]{}
