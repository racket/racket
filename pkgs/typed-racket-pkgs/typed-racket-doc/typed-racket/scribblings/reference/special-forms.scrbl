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


@(module def-racket racket/base
   (require (for-label racket/base) scribble/manual)

   (define for-id (racket for))
   (define for*-id (racket for*))
   (define mod-beg-id (racket #%module-begin))
   (define with-handlers-id (racket with-handlers))
   (define default-continuation-prompt-tag-id
     (racket default-continuation-prompt-tag))
   (define lambda-id (racket lambda))
   (define λ-id (racket λ))
   (define let-id (racket let))
   (define letrec-id (racket letrec))
   (define let*-id (racket let*))
   (define let-values-id (racket let-values))
   (define letrec-values-id (racket letrec-values))
   (define let*-values-id (racket let*-values))
   (define let/cc-id (racket let/cc))
   (define let/ec-id (racket let/ec))
   (define define-id (racket define))
   (define do-id (racket do))
   (define struct-id (racket struct))
   (define define-struct-id (racket define-struct))

   (provide (all-defined-out)))

@(require 'def-racket)

@title[#:tag "special-forms"]{Special Form Reference}

Typed Racket provides a variety of special forms above and beyond
those in Racket.  They are used for annotating variables with types,
creating new types, and annotating expressions.

@section{Binding Forms}

@racket[_loop], @racket[_f], @racket[_a], and @racket[_var] are names,
@racket[_type] is a type.
@racket[_e] is an expression and @racket[_body] is a block.

@defform*[[(let maybe-tvars (binding ...) . body)
           (let loop maybe-ret (binding ...) . body)]
          #:grammar
          ([binding [var e]
                    [var : type e]]
           [maybe-tvars (code:line)
                        (code:line #:forall (tvar ...))
                        (code:line #:∀ (tvar ...))]
           [maybe-ret (code:line)
                      (code:line : type0)])]{
Local bindings, like @|let-id|, each with
associated types.  In the second form, @racket[_type0] is the type of the
result of @racket[_loop] (and thus the result of the entire
expression as well as the final expression in @racket[body]).
Type annotations are optional.

@ex[(: filter-even : (-> (Listof Natural) (Listof Natural) (Listof Natural)))
    (define (filter-even lst accum)
      (if (null? lst)
          accum
          (let ([first : Natural (car lst)]
                [rest  : (Listof Natural) (cdr lst)])
                (if (even? first)
                    (filter-even rest (cons first accum))
                    (filter-even rest accum)))))
    (filter-even (list 1 2 3 4 5 6) null)]

@ex[(: filter-even-loop (-> (Listof Natural) (Listof Natural)))
    (define (filter-even-loop lst)
      (let loop : (Listof Natural)
           ([accum : (Listof Natural) null]
            [lst   : (Listof Natural) lst])
           (cond
             [(null? lst)       accum]
             [(even? (car lst)) (loop (cons (car lst) accum) (cdr lst))]
             [else              (loop accum (cdr lst))])))
    (filter-even-loop (list 1 2 3 4))]

If polymorphic type variables are provided, they are bound in the type
expressions for variable bindings.

@ex[(let #:forall (A) ([x : A 0]) x)]
}

@deftogether[[
@defform[(letrec (binding ...) . body)]
@defform[(let* (binding ...) . body)]
@defform[(let-values ([(var+type ...) e] ...) . body)]
@defform[(letrec-values ([(var+type ...) e] ...) . body)]
@defform[(let*-values ([(var+type ...) e] ...) . body)]]]{

Type-annotated versions of @|letrec-id|, @|let*-id|, @|let-values-id|,
@|letrec-values-id|, and @|let*-values-id|. As with
@racket[let], type annotations are optional.}

@deftogether[[
@defform[(let/cc v : t . body)]
@defform[(let/ec v : t . body)]]]{
  Type-annotated versions of @|let/cc-id| and @|let/ec-id|. As with
  @racket[let], the type annotation is optional.
}

@section{Anonymous Functions}

@defform[(lambda maybe-tvars formals maybe-ret . body)
         #:grammar
         ([formals (formal ...)
                   (formal ... . rst)]
          [formal var
                  [var default-expr]
                  [var : type]
                  [var : type default-expr]
                  (code:line keyword var)
                  (code:line keyword [var : type])
                  (code:line keyword [var : type default-expr])]
          [rst var
               [var : type *]
               [var : type ooo bound]]
          [maybe-tvars (code:line)
                       (code:line #:forall (tvar ...))
                       (code:line #:∀ (tvar ...))
                       (code:line #:forall (tvar ... ooo))
                       (code:line #:∀ (tvar ... ooo))]
          [maybe-ret (code:line)
                     (code:line : type)])]{

Constructs an anonymous function like the @|lambda-id| form from
@racketmodname[racket/base], but allows type annotations on the formal
arguments. If a type annotation is left out, the formal will have
the type @racket[Any].

@ex[
  (lambda ([x : String]) (string-append x "bar"))
  (lambda (x [y : Integer]) (add1 y))
  (lambda (x) x)
]

Type annotations may also be specified for keyword and optional arguments:

@ex[
  (lambda ([x : String "foo"]) (string-append x "bar"))
  (lambda (#:x [x : String]) (string-append x "bar"))
  (lambda (x #:y [y : Integer 0]) (add1 y))
  (lambda ([x 'default]) x)
]

The @racket[lambda] expression may also specify polymorphic type variables
that are bound for the type expressions in the formals.

@ex[
  (lambda #:forall (A) ([x : A]) x)
  (lambda #:∀ (A) ([x : A]) x)
]

In addition, a type may optionally be specified for the @gtech{rest argument}
with either a uniform type or using a polymorphic type. In the former case,
the rest argument is given the type @racket[(Listof type)] where @racket[type]
is the provided type annotation.

@ex[
  (lambda (x . rst) rst)
  (lambda (x rst : Integer *) rst)
  (lambda #:forall (A ...) (x rst : A ... A) rst)
]
}

@defform[(λ formals . body)]{
An alias for the same form using @racket[lambda].}

@defform[(case-lambda maybe-tvars [formals body] ...)]{

A function of multiple arities. The @racket[_formals] are identical
to those accepted by the @racket[lambda] form except that keyword
and optional arguments are not allowed.

Polymorphic type variables, if provided, are bound in the type
expressions in the formals.

Note that each @racket[formals] must have a different arity.

@ex[(define add-map
      (case-lambda
       [([lst : (Listof Integer)])
        (map add1 lst)]
       [([lst1 : (Listof Integer)]
         [lst2 : (Listof Integer)])
        (map + lst1 lst2)]))]

To see how to declare a type for @racket[add-map], see the
@racket[case->] type constructor.}

@section{Loops}

@defform/subs[(for type-ann-maybe (for-clause ...)
                expr ...+)
              ([type-ann-maybe code:blank
                               @code:line[: u]]
               [for-clause [id : t seq-expr]
                           [(binding ...) seq-expr]
                           [id seq-expr]
                           @code:line[#:when guard]]
               [binding id
                        [id : t]])]{
Like @|for-id| from @racketmodname[racket/base], but each @racket[id] has the associated type
@racket[t]. Since the return type is always @racket[Void], annotating
the return type of a @racket[for] form is optional.
Type annotations in clauses are optional for all @racket[for]
variants.
}

@deftogether[[
@defform[(for/list type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/hash type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/hasheq type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/hasheqv type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/vector type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/flvector type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/extflvector type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/and type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/or   type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/first type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/last type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/sum type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for/product type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/list type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/hash type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/hasheq type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/hasheqv type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/vector type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/flvector type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/extflvector type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/and type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/or   type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/first type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/last type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/sum type-ann-maybe (for-clause ...) expr ...+)]
@defform[(for*/product type-ann-maybe (for-clause ...) expr ...+)]
]]{
These behave like their non-annotated counterparts, with the exception
that @racket[#:when] clauses can only appear as the last
@racket[for-clause]. The return value of the entire form must be of
type @racket[u]. For example, a @racket[for/list] form would be
annotated with a @racket[Listof] type. All annotations are optional.
}

@deftogether[[
@defform[(for/lists type-ann-maybe ([id : t] ...)
           (for-clause ...)
           expr ...+)]
@defform[(for/fold  type-ann-maybe ([id : t init-expr] ...)
           (for-clause ...)
           expr ...+)]]]{
These behave like their non-annotated counterparts. Unlike the above,
@racket[#:when] clauses can be used freely with these.
}

@deftogether[[
@defform[(for* void-ann-maybe (for-clause ...)
           expr ...+)]
@defform[(for*/lists type-ann-maybe ([id : t] ...)
           (for-clause ...)
           expr ...+)]
@defform[(for*/fold  type-ann-maybe ([id : t init-expr] ...)
           (for-clause ...)
           expr ...+)]]]{
These behave like their non-annotated counterparts.
}

@defform/subs[(do : u ([id : t init-expr step-expr-maybe] ...)
                      (stop?-expr finish-expr ...)
                expr ...+)
              ([step-expr-maybe code:blank
                                step-expr])]{
Like @|do-id| from @racketmodname[racket/base], but each @racket[id] having the associated type @racket[t], and
the final body @racket[expr] having the type @racket[u]. Type
annotations are optional.
}


@section{Definitions}

@defform*[[(define maybe-tvars v maybe-ann e)
           (define maybe-tvars header maybe-ann . body)]
          #:grammar
          ([header (function-name . formals)
                   (header . formals)]
           [formals (formal ...)
                    (formal ... . rst)]
           [formal var
                   [var default-expr]
                   [var : type]
                   [var : type default-expr]
                   (code:line keyword var)
                   (code:line keyword [var : type])
                   (code:line keyword [var : type default-expr])]
           [rst var
                [var : type *]
                [var : type ooo bound]]
           [maybe-tvars (code:line)
                        (code:line #:forall (tvar ...))
                        (code:line #:∀ (tvar ...))
                        (code:line #:forall (tvar ... ooo))
                        (code:line #:∀ (tvar ... ooo))]
           [maybe-ann (code:line)
                      (code:line : type)])]{

Like @|define-id| from @racketmodname[racket/base], but allows optional
type annotations for the variables.

The first form defines a variable @racket[v] to the result of evaluating
the expression @racket[e]. The variable may have an optional type annotation.

@ex[
  (define foo "foo")
  (define bar : Integer 10)
]

If polymorphic type variables are provided, then they are bound for use
in the type annotation.

@ex[
  (define #:forall (A) mt-seq : (Sequenceof A) empty-sequence)
]

The second form allows the definition of functions with optional
type annotations on any variables. If a return type annotation is
provided, it is used to check the result of the function.

Like @racket[lambda], optional and keyword arguments are supported.

@ex[
    (define (add [first : Integer]
                 [rest  : Integer]) : Integer
      (+ first rest))

    (define #:forall (A)
            (poly-app [func : (A A -> A)]
                      [first : A]
                      [rest  : A]) : A
      (func first rest))]

The function definition form also allows curried function arguments with
corresponding type annotations.

@ex[
  (define ((addx [x : Number]) [y : Number]) (+ x y))
  (define add2 (addx 2))
  (add2 5)
]

Note that unlike @|define-id| from @racketmodname[racket/base], @racket[define]
does not bind functions with keyword arguments to static information about
those functions.
}



@section{Structure Definitions}
@defform/subs[
(struct maybe-type-vars name-spec ([f : t] ...) options ...)
([maybe-type-vars code:blank (v ...)]
 [name-spec name (code:line name parent)]
 [options #:transparent #:mutable])]{
 Defines a @rtech{structure} with the name @racket[name], where the
 fields @racket[f] have types @racket[t], similar to the behavior of @|struct-id|
 from @racketmodname[racket/base].
  When @racket[parent] is present, the
structure is a substructure of @racket[parent].  When
@racket[maybe-type-vars] is present, the structure is polymorphic in the type
 variables @racket[v]. If @racket[parent] is also a polymorphic struct, then
there must be at least as many type variables as in the parent type, and the
parent type is instantiated with a prefix of the type variables matching the
amount it needs.

Options provided have the same meaning as for the @|struct-id| form
from @racketmodname[racket/base].}


@defform/subs[
(define-struct maybe-type-vars name-spec ([f : t] ...) options ...)
([maybe-type-vars code:blank (v ...)]
 [name-spec name (name parent)]
 [options #:transparent #:mutable])]{Legacy version of @racket[struct],
corresponding to @|define-struct-id| from @racketmodname[racket/base].}

@defform/subs[
(define-struct/exec name-spec ([f : t] ...) [e : proc-t])
([name-spec name (name parent)])]{
 Like @racket[define-struct], but defines a procedural structure.
 The procdure @racket[e] is used as the value for @racket[prop:procedure], and must have type @racket[proc-t].}

@section{Names for Types}
@defform*[[(define-type name t maybe-omit-def)
           (define-type (name v ...) t maybe-omit-def)]
          #:grammar
          [(maybe-omit-def (code:line #:omit-define-syntaxes) (code:line))]]{
The first form defines @racket[name] as type, with the same meaning as
@racket[t].  The second form is equivalent to
@racket[(define-type name (All (v ...) t))].  Type names may
refer to other types defined in the same or enclosing scopes.

@ex[(define-type IntStr (U Integer String))
    (define-type (ListofPairs A) (Listof (Pair A A)))]

If @racket[#:omit-define-syntaxes] is specified, no definition of
@racket[name] is created. In this case, some other definition of @racket[name]
is necessary.

If the body of the type definition refers to itself, then the
type definition is recursive. Recursion may also occur mutually,
if a type refers to a chain of other types that eventually refers
back to itself.

@ex[(define-type BT (U Number (Pair BT BT)))
    (let ()
      (define-type (Even A) (U Null (Pairof A (Odd A))))
      (define-type (Odd A) (Pairof A (Even A)))
      (: even-lst (Even Integer))
      (define even-lst '(1 2))
      even-lst)]

However, the recursive reference may not occur immediately inside
the type:

@ex[(define-type Foo Foo)
    (define-type Bar (U Bar False))]
}

@section{Generating Predicates Automatically}

@defform[(make-predicate t)]{

Evaluates to a predicate for the type @racket[t], with the type
@racket[(Any -> Boolean : t)]. @racket[t] may not contain function types, or
types that may refer to mutable data such as @racket[(Vectorof Integer)].} 

@defform[(define-predicate name t)]{
Equivalent to @racket[(define name (make-predicate t))].

@section{Type Annotation and Instantiation}

@defform*[((: v t)
           (: v : t))]{
This declares that @racket[v] has type @racket[t].
The definition of @racket[v] must appear after this declaration.  This
can be used anywhere a definition form may be used.
@ex[(: var1 Integer)
    (: var2 String)]

The second form allows type annotations to elide one level of parentheses
for function types.

@ex[(: var3 : -> Integer)
    (: var4 : String -> Integer)]
}

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

@defform*[[(inst e t ...)
           (inst e t ... t ooo bound)]]{
Instantiate the type of @racket[e] with types @racket[t ...] or with the
poly-dotted types @racket[t ... t ooo bound]. @racket[e] must
have a polymorphic type that can be applied to the supplied number of type
variables. This is legal only in expression contexts.
@ex[(foldl (inst cons Integer Integer) null (list 1 2 3 4))

    (: fold-list : (All (A) (Listof A) -> (Listof A)))
    (define (fold-list lst)
      (foldl (inst cons A A) null lst))

    (fold-list (list "1" "2" "3" "4"))

    (: my-values : (All (A B ...) (A B ... -> (values A B ... B))))
    (define (my-values arg . args)
      (apply (inst values A B ... B) arg args))]}

@defform/none[#{e |@| t ...}]{
A reader abbreviation for @racket[(inst e t ...)].}
@defform/none[#{e |@| t ... t ooo bound}]{
A reader abbreviation for @racket[(inst e t ... t ooo bound)].}

@section{Require}

Here, @racket[_m] is a module spec, @racket[_pred] is an identifier
naming a predicate, and @racket[_maybe-renamed] is an
optionally-renamed identifier.

@defform/subs[#:literals (struct)
(require/typed m rt-clause ...)
([rt-clause [maybe-renamed t]
            [#:struct name ([f : t] ...)
                 struct-option ...]
            [#:struct (name parent) ([f : t] ...)
                 struct-option ...]
            [#:opaque t pred]]
 [maybe-renamed id
                (orig-id new-id)]
 [struct-option
   (code:line #:constructor-name constructor-id)
   (code:line #:extra-constructor-name constructor-id)])]
This form requires identifiers from the module @racket[m], giving
them the specified types.

The first case requires @racket[_maybe-renamed], giving it type
@racket[t].

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
                     [#:struct IntTree
                       ([elem  : Integer]
                        [left  : IntTree]
                        [right : IntTree])]))]

@index["opaque"]{The fourth case} defines a new type @racket[t].  @racket[pred], imported from
module @racket[m], is a predicate for this type.  The type is defined
as precisely those values to which @racket[pred] produces
@racket[#t].  @racket[pred] must have type @racket[(Any -> Boolean)].
Opaque types must be required lexically before they are used.

@ex[(require/typed racket/base
                   [#:opaque Evt evt?]
                   [alarm-evt (Real -> Evt)]
                   [sync (Evt -> Any)])
    evt?
    (sync (alarm-evt (+ 100 (current-inexact-milliseconds))))]

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
Uses outside of a module top-level raise an error.

@ex[(module evts typed/racket
      (require/typed/provide racket/base
                             [#:opaque Evt evt?]
                             [alarm-evt (Real -> Evt)]
                             [sync (Evt -> Any)]))
    (require 'evts)
    (sync (alarm-evt (+ 100 (current-inexact-milliseconds))))]
}

@section{Other Forms}

@defidform[with-handlers]{
Identical to @|with-handlers-id| from @racketmodname[racket/base]
but provides additional annotations to assist the typechecker.
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
