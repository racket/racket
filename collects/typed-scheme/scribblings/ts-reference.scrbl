#lang scribble/manual

@begin[(require "utils.rkt" scribble/eval scriblib/footnote
                racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           (only-in racket/base for)
                           racket/list srfi/14
                           version/check))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))

@title[#:tag "top"]{The Typed Racket Reference} 

@author["Sam Tobin-Hochstadt"]

@(defmodulelang* (typed/racket/base typed/racket)
                 #:use-sources 
                    (typed-scheme/typed-scheme
                     typed-scheme/private/prims
                     typed-scheme/private/base-types 
                     typed-scheme/private/base-types-extra))

@section[#:tag "type-ref"]{Type Reference}

@defidform[Any]{Any Racket value. All other types are subtypes of @racket[Any].}

@defidform[Nothing]{The empty type.  No values inhabit this type, and
any expression of this type will not evaluate to a value.}

@subsection{Base Types}

@(define-syntax-rule                    
   (defnums (ids ...) . rest)
   (deftogether ((defidform ids) ...) . rest))

@subsubsection{Numeric Types}

@defnums[(
Number
Complex
Float-Complex
Real
Float
Nonnegative-Float
Inexact-Real
Exact-Rational
Integer
Natural
Exact-Nonnegative-Integer
Exact-Positive-Integer
Fixnum
Nonnegative-Fixnum
Positive-Fixnum
Zero
Byte
Exact-Number
Float-Negative-Zero
Float-Positive-Zero
Float-Zero
Flonum
Flonum-Negative-Zero
Flonum-Positive-Zero
Flonum-Zero
Index
Inexact-Complex
Inexact-Real-Negative-Zero
Inexact-Real-Positive-Zero
Inexact-Real-Zero
Negative-Exact-Rational
Negative-Float
Negative-Flonum
Negative-Inexact-Real
Negative-Integer
Negative-Real
Negative-Single-Flonum
Nonnegative-Exact-Rational
Nonnegative-Flonum
Nonnegative-Inexact-Real
Nonnegative-Real
Nonnegative-Single-Flonum
Nonpositive-Exact-Rational
Nonpositive-Fixnum
Nonpositive-Float
Nonpositive-Flonum
Nonpositive-Inexact-Real
Nonpositive-Integer
Nonpositive-Real
Nonpositive-Single-Flonum
One
Positive-Byte
Positive-Exact-Rational
Positive-Float
Positive-Flonum
Positive-Index
Positive-Inexact-Real
Positive-Integer
Positive-Real
Positive-Single-Flonum
Real-Zero
Single-Flonum
Single-Flonum-Complex
Single-Flonum-Negative-Zero
Single-Flonum-Positive-Zero
Single-Flonum-Zero
)]{These types represent the hierarchy of @rtech{numbers} of Racket.
@racket[Integer] includes only @rtech{integers} that are @rtech{exact
numbers}, corresponding to the predicate @racket[exact-integer?].
@racket{Real} includes both exact and inexact reals.
An @racket{Inexact-Real} can be either 32- or 64-bit floating-point
numbers. @racket{Float} is restricted to 64-bit floats, which are the
default in Racket.

@ex[
7
8.3
(/ 8 3)
0
-12
3+4i]
}

@subsubsection{Other Base Types}

@deftogether[(
@defidform[Boolean]
@defidform[True]
@defidform[False]
@defidform[String]
@defidform[Keyword]
@defidform[Symbol]
@defidform[Void]
@defidform[Input-Port]
@defidform[Output-Port]
@defidform[Port]
@defidform[Path]
@defidform[Path-String]
@defidform[Regexp]
@defidform[PRegexp]
@defidform[Byte-Regexp]
@defidform[Byte-PRegexp]
@defidform[Bytes]
@defidform[Namespace]
@defidform[Null]
@defidform[EOF]
@defidform[Continuation-Mark-Set]
@defidform[Char]
@defidform[Undefined]
@defidform[Module-Path]
@defidform[Module-Path-Index]
@defidform[Compiled-Module-Expression]
@defidform[Resolved-Module-Path]
@defidform[Thread])]{
These types represent primitive Racket data.

@ex[
#t
#f
"hello"
(current-input-port)
(current-output-port)
(string->path "/")
#rx"a*b*"
#px"a*b*"
'#"bytes"
(current-namespace)
#\b
(thread (lambda () (add1 7)))
]
}

@subsection{Singleton Types}

Some kinds of data are given singleton types by default.  In
particular, @rtech{booleans}, @rtech{symbols}, and @rtech{keywords} have types which
consist only of the particular boolean, symbol, or keyword.  These types are
subtypes of @racket[Boolean], @racket[Symbol] and @racket[Keyword], respectively.

@ex[
#t
'#:foo
'bar
]

@subsection{Containers}


The following base types are parameteric in their type arguments.

@defform[(Pairof s t)]{is the @rtech{pair} containing @racket[s] as the @racket[car]
  and @racket[t] as the @racket[cdr]}

@ex[
(cons 1 2)
(cons 1 "one")
]


@defform[(Listof t)]{Homogenous @rtech{lists} of @racket[t]}
@defform[(List t ...)]{is the type of the list with one element, in order, 
  for each type provided to the @racket[List] type constructor.}
@defform/none[(#,(racket List) t ... trest #,(racket ...) bound)]{is the type of a list with
one element for each of the @racket[t]s, plus a sequence of elements
corresponding to @racket[trest], where @racket[bound]
  must be an identifier denoting a type variable bound with @racket[...].}
@defform[(List* t t1 ... s)]{is equivalent to @racket[(Pairof t (List* t1 ... s))].}

@ex[
(list 'a 'b 'c)
(map symbol->string (list 'a 'b 'c))
]

@defform[(MListof t)]{Homogenous @rtech{mutable lists} of @racket[t].}
@defform[(MPairof t u)]{@rtech{Mutable pairs} of @racket[t] and @racket[u].}

@defform[(Boxof t)]{A @rtech{box} of @racket[t]}

@ex[(box "hello world")]

@defform[(Vectorof t)]{Homogenous @rtech{vectors} of @racket[t]}
@defform[(Vector t ...)]{is the type of the list with one element, in order, 
  for each type provided to the @racket[Vector] type constructor.}
@defidform[FlVector]{An @rtech{flvector}.}

@ex[(vector 1 2 3)
#(a b c)]

@defform[(HashTable k v)]{is the type of a @rtech{hash table} with key type
   @racket[k] and value type @racket[v].

@ex[#hash((a . 1) (b . 2))]
}

@defform[(Setof t)]{is the type of a @rtech{set} of @racket[t].
@ex[(set 0 1 2 3)]
}

@defform[(Channelof t)]{A @rtech{channel} on which only @racket[t]s can be sent.
@ex[
(ann (make-channel) (Channelof Symbol))
]
}

@defform*[[(Parameterof t)
           (Parameterof s t)]]{A @rtech{parameter} of @racket[t].  If two type arguments are supplied, 
                                 the first is the type the parameter accepts, and the second is the type returned.
@ex[current-input-port
    current-directory]
}
                              
@defform[(Promise t)]{A @rtech{promise} of @racket[t].
 @ex[(delay 3)]}

@defform[(Futureof t)]{A @rtech{future} which produce a value of type @racket[t] when touched.}

@defform[(Sequenceof t ...)]{A @rtech{sequence} that produces values of the
types @racket[_t ...] on each iteration.}

@subsection{Syntax Objects}

The following types represent @rtech{syntax object}s and their content.

@defform[(Syntaxof t)]{A syntax object with content of type @racket[t].
Applying @racket[syntax-e] to a value of type @racket[(Syntaxof t)] produces a
value of type @racket[t].}

@defidform[Identifier]{A syntax object containing a @rtech{symbol}.  Equivalent
to @racket[(Syntaxof Symbol)].}

@defidform[Syntax]{A syntax object containing only @rtech{symbol}s,
@rtech{keyword}s, @rtech{string}s, @rtech{character}s, @rtech{boolean}s,
@rtech{number}s, @rtech{box}es containing @racket[Syntax], @rtech{vector}s of
@racket[Syntax], or (possibly improper) @rtech{list}s of @racket[Syntax].
Equivalent to @racket[(Syntaxof Syntax-E)].}

@defidform[Syntax-E]{The content of syntax objects of type @racket[Syntax].
Applying @racket[syntax-e] to a value of type @racket[Syntax] produces a value
of type @racket[Syntax-E].}

@defform[(Sexpof t)]{The recursive union of @racket[t] with @rtech{symbol}s,
@rtech{keyword}s, @rtech{string}s, @rtech{character}s, @rtech{boolean}s,
@rtech{number}s, @rtech{box}es, @rtech{vector}s, and (possibly improper)
@rtech{list}s.}

@defidform[Sexp]{Applying @racket[syntax->datum] to a value of type
@racket[Syntax] produces a value of type @racket[Sexp].  Equivalent to
@racket[(Sexpof Nothing)].}

@defidform[Datum]{Applying @racket[datum->syntax] to a value of type
@racket[Datum] produces a value of type @racket[Syntax].  Equivalent to
@racket[(Sexpof Syntax)].}

@defform[(Ephemeronof t)]{An @rtech{ephemeron} whose value is of type @racket[t].}

@subsection{Other Type Constructors} 

@defform*[#:id -> #:literals (* ...)
	       [(dom ... -> rng)
	        (dom ... rest * -> rng)
		(dom ... rest #,(racket ...) bound -> rng)
                (dom -> rng : pred)]]{is the type of functions from the (possibly-empty)
  sequence @racket[dom ...] to the @racket[rng] type.  The second form
  specifies a uniform rest argument of type @racket[rest], and the
  third form specifies a non-uniform rest argument of type
  @racket[rest] with bound @racket[bound].  In the third form, the
  second occurrence of @racket[...] is literal, and @racket[bound]
  must be an identifier denoting a type variable. In the fourth form, 
  there must be only one @racket[dom] and @racket[pred] is the type 
  checked by the predicate.
  
  @ex[(λ: ([x : Number]) x)
      (λ: ([x : Number] . [y : String *]) (length y))
      ormap
      string?]}

@defidform[Procedure]{is the supertype of all function types.}


@defform[(U t ...)]{is the union of the types @racket[t ...].
 @ex[(λ: ([x : Real])(if (> 0 x) "yes" 'no))]}
@defform[(case-> fun-ty ...)]{is a function that behaves like all of
  the @racket[fun-ty]s, considered in order from first to last.  The @racket[fun-ty]s must all be function
  types constructed with @racket[->].
  @ex[(: add-map : (case->
                     [(Listof Integer) -> (Listof Integer)]
                     [(Listof Integer) (Listof Integer) -> (Listof Integer)]))]
  For the definition of @racket[add-map] look into @racket[case-lambda:].}

@defform/none[(t t1 t2 ...)]{is the instantiation of the parametric type
  @racket[t] at types @racket[t1 t2 ...]}
@defform[(All (v ...) t)]{is a parameterization of type @racket[t], with
  type variables @racket[v ...].  If @racket[t] is a function type
      constructed with @racket[->], the outer pair of parentheses
      around the function type may be omitted.
      @ex[(: list-lenght : (All (A) (Listof A) -> Natural))
          (define (list-lenght lst)
            (if (null? lst)
                0
                (add1 (list-lenght (cdr lst)))))]}

@defform[(Values t ...)]{is the type of a sequence of multiple values, with
types @racket[t ...].  This can only appear as the return type of a
function.
@ex[(values 1 2 3)]}
@defform/none[v]{where @racket[v] is a number, boolean or string, is the singleton type containing only that value}
@defform/none[(quote val)]{where @racket[val] is a Racket value, is the singleton type containing only that value}
@defform/none[i]{where @racket[i] is an identifier can be a reference to a type
name or a type variable}
@defform[(Rec n t)]{is a recursive type where @racket[n] is bound to the
recursive type in the body @racket[t]
@ex[(define-type IntList (Rec List (Pair Integer (U List Null))))
    
    (define-type (List A) (Rec List (Pair A (U List Null))))]}

@(define-syntax-rule (defalias id1 id2)
                     @defidform[id1]{An alias for @racket[id2].})

@defalias[→ ->]
@defalias[∀ All]

@subsection{Other Types}

@defform[(Option t)]{Either @racket[t] or @racket[#f]}
@defform[(Opaque t)]{A type constructed using @racket[require-opaque-type].}
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
				expression in @racket[body]).
				Type annotations are optional.
@ex[(: filter-even : (Listof Natural) (Listof Natural) -> (Listof Natural))
    (define (filter-even lst accum)
      (if (null? lst)
          accum
          (let: ([first : Natural (car lst)]
                 [rest  : (Listof Natural) (cdr lst)])
                (if (even? first)
                    (filter-even rest (cons first accum))
                    (filter-even rest accum)))))
    (filter-even (list 1 2 3 4 5 6) null)]

@ex[(: filter-even-loop : (Listof Natural) -> (Listof Natural))
    (define (filter-even-loop lst)
      (let: loop : (Listof Natural) 
            ([accum : (Listof Natural) null]
             [lst   : (Listof Natural) lst])
            (cond 
              [(null? lst)       accum]
              [(even? (car lst)) (loop (cons (car lst) accum) (cdr lst))]
              [else              (loop accum (cdr lst))])))
    (filter-even-loop (list 1 2 3 4))]}

@deftogether[[
@defform[(letrec: ([v : t e] ...) . body)]
@defform[(let*: ([v : t e] ...) . body)]
@defform[(let-values: ([([v : t] ...) e] ...) . body)]
@defform[(letrec-values: ([([v : t] ...) e] ...) . body)]
@defform[(let*-values: ([([v : t] ...) e] ...) . body)]]]{
Type-annotated versions of
@racket[letrec], @racket[let*], @racket[let-values],
@racket[letrec-values], and @racket[let*-values]. As with
@racket[let:], type annotations are optional.}

@deftogether[[
@defform[(let/cc: v : t . body)]
@defform[(let/ec: v : t . body)]]]{Type-annotated versions of
@racket[let/cc] and @racket[let/ec].}

@subsection{Anonymous Functions}

@defform/subs[(lambda: formals . body)
([formals ([v : t] ...) 
	  ([v : t] ... . [v : t *])
	  ([v : t] ... . [v : t ...])])]{
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
different arity.
@ex[(define add-map 
      (case-lambda:
       [([lst : (Listof Integer)])
        (map add1 lst)]
       [([lst1 : (Listof Integer)]
         [lst2 : (Listof Integer)])
        (map + lst1 lst2)]))] 
For the type declaration of @racket[add-map] look at @racket[case-lambda].}

@defform[(pcase-lambda: (a ...) [formals body] ...)]{
A polymorphic function of multiple arities.}
@defform/subs[(opt-lambda: formals . body)
([formals ([v : t] ... [v : t default] ...)
          ([v : t] ... [v : t default] ... . [v : t *])
	  ([v : t] ... [v : t default] ... . [v : t ...])])]{
A function with optional arguments.}
@defform[(popt-lambda: (a ...) formals . body)]{
A polymorphic function with optional arguments.}


@subsection{Loops}

@defform/subs[(for: type-ann-maybe (for-clause ...)
                expr ...+)
              ([type-ann-maybe code:blank
                               @code:line[: Void]]
	       [for:-clause [id : t seq-expr]
	                    [id seq-expr]
	                    @code:line[#:when guard]])]{
Like @racket[for], but each @racket[id] having the associated type
@racket[t]. Since the return type is always @racket[Void], annotating
the return type of a @racket[for] form is optional. Unlike
@racket[for], multi-valued @racket[seq-expr]s are not supported.
Type annotations in clauses are optional for all @racket[for:]
variants.
}

@deftogether[[
@defform[(for/list: : u (for:-clause ...) expr ...+)]
@defform[(for/hash: : u (for:-clause ...) expr ...+)]
@defform[(for/hasheq: : u (for:-clause ...) expr ...+)]
@defform[(for/hasheqv: : u (for:-clause ...) expr ...+)]
@defform[(for/vector: : u (for:-clause ...) expr ...+)]
@defform[(for/flvector: : u (for:-clause ...) expr ...+)]
@defform[(for/and: : u (for:-clause ...) expr ...+)]
@defform[(for/or:   : u (for:-clause ...) expr ...+)]
@defform[(for/first: : u (for:-clause ...) expr ...+)]
@defform[(for/last: : u (for:-clause ...) expr ...+)]
@defform[(for*/list: : u (for:-clause ...) expr ...+)]
@defform[(for*/hash: : u (for:-clause ...) expr ...+)]
@defform[(for*/hasheq: : u (for:-clause ...) expr ...+)]
@defform[(for*/hasheqv: : u (for:-clause ...) expr ...+)]
@defform[(for*/vector: : u (for:-clause ...) expr ...+)]
@defform[(for*/flvector: : u (for:-clause ...) expr ...+)]
@defform[(for*/and: : u (for:-clause ...) expr ...+)]
@defform[(for*/or:   : u (for:-clause ...) expr ...+)]
@defform[(for*/first: : u (for:-clause ...) expr ...+)]
@defform[(for*/last: : u (for:-clause ...) expr ...+)]
]]{
These behave like their non-annotated counterparts, with the exception
that @racket[#:when] clauses can only appear as the last
@racket[for:-clause]. The last @racket[expr] of the body must have
type @racket[u].
}

@deftogether[[
@defform[(for/lists: : u ([id : t] ...)
           (for:-clause ...)
	   expr ...+)]
@defform[(for/fold:  : u ([id : t init-expr] ...)
	   (for:-clause ...)
	   expr ...+)]]]{
These behave like their non-annotated counterparts. Unlike the above,
@racket[#:when] clauses can be used freely with these.
}

@deftogether[[
@defform[(for*: type-ann-maybe (for-clause ...)
           expr ...+)]
@defform[(for*/lists: : u ([id : t] ...)
           (for:-clause ...)
	   expr ...+)]
@defform[(for*/fold:  : u ([id : t init-expr] ...)
	   (for:-clause ...)
	   expr ...+)]]]{
These behave like their non-annotated counterparts.
}

@defform/subs[(do: : u ([id : t init-expr step-expr-maybe] ...)
                       (stop?-expr finish-expr ...)
                expr ...+)
              ([step-expr-maybe code:blank
                                step-expr])]{
Like @racket[do], but each @racket[id] having the associated type @racket[t], and 
the final body @racket[expr] having the type @racket[u]. Type
annotations are optional.
}


@subsection{Definitions}

@defform*[[(define: v : t e)
	   (define: (f . formals) : t . body)	   
	   (define: (a ...) (f . formals) : t . body)]]{
These forms define variables, with annotated types.  The first form
defines @racket[v] with type @racket[t] and value @racket[e].  The
second and third forms defines a function @racket[f] with appropriate
types.  In most cases, use of @racket[:] is preferred to use of @racket[define:].

@ex[(define: foo : Integer 10)

    (define: (add [first : Integer]
                  [rest  : Integer]) : Integer 
      (+ first rest))
    
    (define: (A) (poly-app [func : (A A -> A)] 
                           [first : A]
                           [rest  : A]) : A 
      (func first rest))]}



@subsection{Structure Definitions}
@defform/subs[
(struct: maybe-type-vars name-spec ([f : t] ...) options ...)
([maybe-type-vars code:blank (v ...)]
 [name-spec name (code:line name parent)]
 [options #:transparent #:mutable])]{
 Defines a @rtech{structure} with the name @racket[name], where the
 fields @racket[f] have types @racket[t], similar to the behavior of @racket[struct].
  When @racket[parent] is present, the
structure is a substructure of @racket[parent].  When
@racket[maybe-type-vars] is present, the structure is polymorphic in the type
 variables @racket[v].

Options provided have the same meaning as for the @racket[struct] form.}


@defform/subs[
(define-struct: maybe-type-vars name-spec ([f : t] ...) options ...)
([maybe-type-vars code:blank (v ...)]
 [name-spec name (name parent)]
 [options #:transparent #:mutable])]{Legacy version of @racket[struct:], 
corresponding to @racket[define-struct].}
                                 
@defform/subs[
(define-struct/exec: name-spec ([f : t] ...) [e : proc-t])
([name-spec name (name parent)])]{
 Like @racket[define-struct:], but defines a procedural structure.  
 The procdure @racket[e] is used as the value for @racket[prop:procedure], and must have type @racket[proc-t].}

@subsection{Names for Types}
@defform*[[(define-type name t)
	   (define-type (name v ...) t)]]{
The first form defines @racket[name] as type, with the same meaning as
@racket[t].  The second form is equivalent to
@racket[(define-type name (All (v ...) t))].  Type names may
refer to other types defined in the same module, but
cycles among them are prohibited.

@ex[(define-type IntStr (U Integer String))
    (define-type (ListofPairs A) (Listof (Pair A A)))]}

@subsection{Generating Predicates Automatically}
@defform[(define-predicate name t)]{
Defines @racket[name] as a predicate for the type @racket[t].
@racket[name] has the type @racket[(Any -> Boolean : t)]. 
@racket[t] may not contain function types.}


@subsection{Type Annotation and Instantiation}

@defform[(: v t)]{This declares that @racket[v] has type @racket[t].
The definition of @racket[v] must appear after this declaration.  This
can be used anywhere a definition form may be used.
@ex[(: var1 Integer)
    (: var2 String)]}

@defform[(provide: [v t] ...)]{This declares that the @racket[v]s have
the types @racket[t], and also provides all of the @racket[v]s.}

@defform/none[@litchar|{ #{v : t} }|]{ This declares that the variable @racket[v] has type
@racket[t].  This is legal only for binding occurrences of @racket[_v].}

@defform[(ann e t)]{Ensure that @racket[e] has type @racket[t], or
some subtype.  The entire expression has type @racket[t].
This is legal only in expression contexts.  The syntax @litchar{#{e :: t}} may
also be used.}

@defform[(inst e t ...)]{Instantiate the type of @racket[e] with types
@racket[t ...].  @racket[e] must have a polymorphic type with the
appropriate number of type variables. This is legal only in expression
contexts.
@ex[(foldl (inst cons Integer Integer) null (list 1 2 3 4))]
        
@ex[(: fold-list : (All (A) (Listof A) -> (Listof A)))
    (define (fold-list lst)
      (foldl (inst cons A A) null lst))
    
    (fold-list (list "1" "2" "3" "4"))]

The syntax @litchar|{#{e @ t ...}}| may also be used.
}

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


@ex[(module UNTYPED racket/base
      (define n 100)
      
      (define-struct IntTree 
        (elem left right))
      
      (provide n (struct-out IntTree)))
    
    (module TYPED typed/racket
      (require/typed 'UNTYPED 
                     [n Natural]
                     [struct IntTree 
                       ([elem  : Integer]
                        [left  : IntTree]
                        [right : IntTree])]))]

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
a @racket[require/typed] form. Here is an example of using 
@racket[case->] in @racket[require/typed].

@(racketblock
  (require/typed racket/base
                 [file-or-directory-modify-seconds 
                  (case->
                    [String -> Exact-Nonnegative-Integer]
                    [String (Option Exact-Nonnegative-Integer) 
                            -> 
                            (U Exact-Nonnegative-Integer Void)]
                    [String (Option Exact-Nonnegative-Integer) (-> Any) 
                            ->
                            Any])]))

@racket[file-or-directory-modify-seconds] has some arguments which are optional, 
so we need to use @racket[case->].}
 
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

@section{Utilities}

Typed Racket provides some additional utility functions to facilitate typed programming.

@defproc*[
([(assert [v (U #f A)]) A]
 [(assert [v A] [p? (A -> Any : B)]) B])]{
Verifies that the argument satisfies the constraint.  If no predicate
is provided, simply checks that the value is not
@racket[#f].  
}

@examples[#:eval the-top-eval
(define: x : (U #f Number) (string->number "7"))
x
(assert x)
(define: y : (U String Number) 0)
y
(assert y number?)
(assert y boolean?)]

@defform*/subs[[(with-asserts ([id maybe-pred] ...) body ...+)]
              ([maybe-pred code:blank
                           (code:line predicate)])]{
Guard the body with assertions. If any of the assertions fail, the
program errors. These assertions behave like @racket[assert].
}


@section{Typed Racket Syntax Without Type Checking}

@defmodulelang*[(typed/racket/no-check
                 typed/racket/base/no-check)]

On occasions where the Typed Racket syntax is useful, but actual
typechecking is not desired, the @racketmodname[typed/racket/no-check]
and @racketmodname[typed/racket/base/no-check] languages are useful.
They provide the same bindings and syntax as
@racketmodname[typed/racket] and @racketmodname[typed/racket/base],
but do no type checking.

Examples:

@racketmod[typed/racket/no-check
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

(fun val)]}

@section{Optimization in Typed Racket}

@note{
See
@secref[#:doc '(lib "typed-scheme/scribblings/ts-guide.scrbl")]{optimization}
in the guide for tips to get the most out of the optimizer.
}

Typed Racket provides a type-driven optimizer that rewrites well-typed
programs to potentially make them faster. It should in no way make
your programs slower or unsafe.

Typed Racket's optimizer is turned on by default. If you want to
deactivate it (for debugging, for instance), you must add the
@racket[#:no-optimize] keyword when specifying the language of your
program:

@racketmod[typed/racket #:no-optimize]

@section{Legacy Forms}

The following forms are provided by Typed Racket for backwards
compatibility.  

@defidform[define-type-alias]{Equivalent to @racket[define-type].}
@defidform[define-typed-struct]{Equivalent to @racket[define-struct:]}
@defidform[require/opaque-type]{Similar to using the @racket[opaque]
keyword with @racket[require/typed].}
@defidform[require-typed-struct]{Similar to using the @racket[struct]
keyword with @racket[require/typed].}
@defidform[pdefine:]{Defines a polymorphic function.}
@defform[(pred t)]{Equivalent to @racket[(Any -> Boolean : t)].}

@defalias[Un U]
@defalias[mu Rec]
@defalias[Tuple List]
@defalias[Parameter Parameterof]
@defalias[Pair Pairof]

@section{Compatibility Languages}

@(defmodulelang*/no-declare (typed/scheme typed/scheme/base typed-scheme))
Typed versions of the @racketmod[scheme] and @racketmod[scheme/base]
languages. The @racketmod[typed-scheme] language is equivalent to the
@racketmod[typed/scheme/base] language.

@(declare-exporting typed/scheme/base typed/scheme typed-scheme
                    #:use-sources 
                    (typed-scheme/typed-scheme
                     typed-scheme/private/prims
                     typed-scheme/private/base-types 
                     typed-scheme/private/base-types-extra))



@section{Experimental Features}

These features are currently experimental and subject to change.

@defform[(Class args ...)]{A type constructor for typing classes created using @racketmodname[racket/class].}
@defform[(Instance c)]{A type constructor for typing objects created using @racketmodname[racket/class].}

@defform[(:type t)]{Prints the type @racket[_t].}

@defform[(declare-refinement id)]{Declares @racket[id] to be usable in
refinement types.}

@defform[(Refinement id)]{Includes values that have been tested with the
predicate @racket[id], which must have been specified with
@racket[declare-refinement].}

@defform[(define-typed-struct/exec forms ...)]{Defines an executable structure.}
