#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket])
                           (only-in racket/base)))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))


@(define-syntax-rule (def-racket for-id for*-id with-handlers-id
                                 default-continuation-prompt-tag-id
                                 mod-beg-id lambda-id λ-id define-id)
  (begin
    (require (for-label (only-in racket/base for for* with-handlers
                                             default-continuation-prompt-tag
                                             #%module-begin lambda λ define)))
    (define for-id (racket for))
    (define for*-id (racket for*))
    (define mod-beg-id (racket #%module-begin))
    (define with-handlers-id (racket with-handlers))
    (define default-continuation-prompt-tag-id
      (racket default-continuation-prompt-tag))
    (define lambda-id (racket lambda))
    (define λ-id (racket λ))
    (define define-id (racket define))))
@(def-racket for-id for*-id with-handlers-id
  default-continuation-prompt-tag-id
  mod-beg-id lambda-id λ-id define-id)


@title[#:tag "special-forms"]{Special Form Reference}

Typed Racket provides a variety of special forms above and beyond
those in Racket.  They are used for annotating variables with types,
creating new types, and annotating expressions.

@section{Binding Forms}

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
@defform[(plet: (a ...) ([v : t e] ...) . body)]{
A polymorphic version of @racket[let:], abstracted over the type variables
@racket[a]. The type variables @racket[a] are bound in both the types
of the formal, and in any type expressions in the @racket[body].
Does not support the looping form of let.}

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

@section{Anonymous Functions}

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


@section{Loops}

@defform/subs[(for: type-ann-maybe (for:-clause ...)
                expr ...+)
              ([type-ann-maybe code:blank
                               @code:line[: u]]
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
@defform[(for/list: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/hash: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/hasheq: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/hasheqv: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/vector: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/flvector: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/and: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/or:   type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/first: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/last: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/sum: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for/product: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/list: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/hash: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/hasheq: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/hasheqv: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/vector: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/flvector: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/and: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/or:   type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/first: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/last: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/sum: type-ann-maybe (for:-clause ...) expr ...+)]
@defform[(for*/product: type-ann-maybe (for:-clause ...) expr ...+)]
]]{
These behave like their non-annotated counterparts, with the exception
that @racket[#:when] clauses can only appear as the last
@racket[for:-clause]. The return value of the entire form must be of
type @racket[u]. For example, a @racket[for/list:] form would be
annotated with a @racket[Listof] type. All annotations are optional.
}

@deftogether[[
@defform[(for/lists: type-ann-maybe ([id : t] ...)
           (for:-clause ...)
           expr ...+)]
@defform[(for/fold:  type-ann-maybe ([id : t init-expr] ...)
           (for:-clause ...)
           expr ...+)]]]{
These behave like their non-annotated counterparts. Unlike the above,
@racket[#:when] clauses can be used freely with these.
}

@deftogether[[
@defform[(for*: void-ann-maybe (for-clause ...)
           expr ...+)]
@defform[(for*/lists: type-ann-maybe ([id : t] ...)
           (for:-clause ...)
           expr ...+)]
@defform[(for*/fold:  type-ann-maybe ([id : t init-expr] ...)
           (for:-clause ...)
           expr ...+)]]]{
These behave like their non-annotated counterparts.
}

@deftogether[[
@defidform[for]
@defidform[for*]]]{
These are identical to @|for-id| and @|for*-id|, but provide additional annotations to help the typechecker.
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


@section{Definitions}

@defform*[[(define: v : t e)
           (define: (f . formals) : t . body)
           (define: (a ...) v : t e)
           (define: (a ...) (f . formals) : t . body)]]{
These forms define variables, with annotated types.  The first form
defines @racket[v] with type @racket[t] and value @racket[e]. The third
form does the same, but allows the specification of type variables.
The second and fourth forms defines a function @racket[f] with appropriate
types. In most cases, use of @racket[:] is preferred to use of @racket[define:].

@ex[(define: foo : Integer 10)

    (define: (add [first : Integer]
                  [rest  : Integer]) : Integer
      (+ first rest))

    (define: (A) mt-seq : (Sequenceof A) empty-sequence)

    (define: (A) (poly-app [func : (A A -> A)]
                           [first : A]
                           [rest  : A]) : A
      (func first rest))]}



@section{Structure Definitions}
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
 variables @racket[v]. If @racket[parent] is also a polymorphic struct, then
there must be at least as many type variables as in the parent type, and the
parent type is instantiated with a prefix of the type variables matching the
amount it needs.

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

@section{Names for Types}
@defform*[[(define-type name t)
           (define-type (name v ...) t)]]{
The first form defines @racket[name] as type, with the same meaning as
@racket[t].  The second form is equivalent to
@racket[(define-type name (All (v ...) t))].  Type names may
refer to other types defined in the same module, but
cycles among them are prohibited.

@ex[(define-type IntStr (U Integer String))
    (define-type (ListofPairs A) (Listof (Pair A A)))]}

@section{Generating Predicates Automatically}

@defform[(make-predicate t)]{

Evaluates to a predicate for the type @racket[t], with the type
@racket[(Any -> Boolean : t)]. @racket[t] may not contain function types, or
types that may refer to mutable data such as @racket[(Vectorof Integer)].} 

@defform[(define-predicate name t)]{
Equivalent to @racket[(define name (make-predicate t))].

@section{Type Annotation and Instantiation}

@defform[(: v t)]{This declares that @racket[v] has type @racket[t].
The definition of @racket[v] must appear after this declaration.  This
can be used anywhere a definition form may be used.
@ex[(: var1 Integer)
    (: var2 String)]}

@defform[(provide: [v t] ...)]{This declares that the @racket[v]s have
the types @racket[t], and also provides all of the @racket[v]s.}

@defform/none[#{v : t}]{ This declares that the variable @racket[v] has type
@racket[t].  This is legal only for binding occurrences of @racket[_v].}

@defform[(ann e t)]{Ensure that @racket[e] has type @racket[t], or
some subtype.  The entire expression has type @racket[t].
This is legal only in expression contexts.}

@defform/none[#{e :: t}]{A reader abbreviation for @racket[(ann e t)].}

@defform[(cast e t)]{The entire expression has the type @racket[t], while
@racket[e] may have any type. The value of the entire expression is the value
returned by @racket[e], protected by a contract ensuring that it has type
@racket[t]. This is legal only in expression contexts.

@ex[(cast 3 Integer)
(cast 3 String)
(cast (lambda: ([x : Any]) x) (String -> String))
]
}

@defform[(inst e t ...)]{Instantiate the type of @racket[e] with types
@racket[t ...].  @racket[e] must have a polymorphic type with the
appropriate number of type variables. This is legal only in expression
contexts.
@ex[(foldl (inst cons Integer Integer) null (list 1 2 3 4))

    (: fold-list : (All (A) (Listof A) -> (Listof A)))
    (define (fold-list lst)
      (foldl (inst cons A A) null lst))

    (fold-list (list "1" "2" "3" "4"))]}

@defform/none[#{e |@| t ...}]{A reader abbreviation for @racket[(inst e t ...)].}

@section{Require}

Here, @racket[_m] is a module spec, @racket[_pred] is an identifier
naming a predicate, and @racket[_r] is an optionally-renamed identifier.

@defform/subs[#:literals (struct)
(require/typed m rt-clause ...)
([rt-clause [r t]
            [#:struct name ([f : t] ...)
                 struct-option ...]
            [#:struct (name parent) ([f : t] ...)
                 struct-option ...]
            [#:opaque t pred]]
 [struct-option
   (code:line #:constructor-name constructor-id)
   (code:line #:extra-constructor-name constructor-id)])]
This form requires identifiers from the module @racket[m], giving
them the specified types.

The first case requires @racket[r], giving it type @racket[t].

@index["struct"]{The second and third cases} require the struct with name @racket[name]
with fields @racket[f ...], where each field has type @racket[t].  The
third case allows a @racket[parent] structure type to be specified.
The parent type must already be a structure type known to Typed
Racket, either built-in or via @racket[require/typed].  The
structure predicate has the appropriate Typed Racket filter type so
that it may be used as a predicate in @racket[if] expressions in Typed
Racket.


@ex[(module UNTYPED racket/base
      (define n 100)

      (struct IntTree
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

Some types, notably the types of predicates such as @racket[number?],
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

@defform[(require/typed/provide m rt-clause ...)]{
Similar to @racket[require/typed], but also provides the imported identifiers.
}

@section{Other Forms}

@deftogether[[@defidform[with-handlers]
              @defidform[lambda]
              @defidform[λ]
              @defidform[define]]]{
Identical to @|with-handlers-id|, @|lambda-id|, @|λ-id|, and @|define-id|, respectively, 
but provide additional annotations to assist the typechecker.  The @racket[define:], 
@racket[lambda:], and @racket[λ:] forms are useful replacements which support type
annotation.
             
Note that unlike @|define-id|, @racket[define] does not bind functions with keyword arguments
to static information about those functions.
}

@defproc[(default-continuation-prompt-tag) (-> (Prompt-Tagof Any (Any -> Any)))]{
  Identical to @|default-continuation-prompt-tag-id|, but additionally protects
  the resulting prompt tag with a contract that wraps
  higher-order values, such as functions, that are communicated with that
  prompt tag. If the wrapped value is used in untyped code, a contract error
  will be raised.

  @ex[
    (module typed typed/racket
      (provide do-abort)
      (: do-abort (-> Void))
      (define (do-abort)
        (abort-current-continuation
         (code:comment "typed, and thus contracted, prompt tag")
         (default-continuation-prompt-tag)
         (λ: ([x : Integer]) (+ 1 x)))))
    (module untyped racket
      (require 'typed)
      (call-with-continuation-prompt
        (λ () (do-abort))
        (default-continuation-prompt-tag)
        (code:comment "the function cannot be passed an argument")
        (λ (f) (f 3))))
    (require 'untyped)
  ]
}

@defform[(#%module-begin form ...)]{

Legal only in a @rtech{module begin context}.
The @racket[#%module-begin] form of @racketmodname[typed/racket] checks all the
forms in the module, using the Typed Racket type checking rules.  All
@racket[provide] forms are rewritten to insert contracts where appropriate.
Otherwise, the @racket[#%module-begin] form of @racketmodname[typed/racket]
behaves like @|mod-beg-id| from @racketmodname[racket].}

@defform[(#%top-interaction . form)]{

Performs type checking of forms entered at the read-eval-print loop.  The
@racket[#%top-interaction] form also prints the type of @racket[form] after
type checking.}


@(close-eval the-eval)
