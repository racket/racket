#lang scribble/manual

@begin[(require "../utils.rkt"
		scribble/core scribble/eval
		(for-label (only-meta-in 0 typed/racket)
                           (only-in mzlib/etc let+)))]

@title[#:tag "more"]{Specifying Types}

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))


The previous section introduced the basics of the Typed Racket type
system.  In this section, we will see several new features of the
language, allowing types to be specified and used.

@section{Type Annotation and Binding Forms}

In general, variables in Typed Racket must be annotated with their
type. A later subsection (@secref["when-annotations?"]) introduces a
heuristic which more precisely details when type annotations are
needed.

@subsection{Annotating Definitions}

We have already seen the @racket[:] type annotation form.  This is
useful for definitions, at both the top level of a module

@racketblock[
(: x Number)
(define x 7)]

and in an internal definition

@racketblock[
(let ()
  (: x Number)
  (define x 7)
  (add1 x))
]

In addition to the @racket[:] form, almost all binding forms from
@racketmodname[racket] have counterparts which allow the specification
of types. The @racket[define:] form allows the definition of variables
in both top-level and internal contexts.

@racketblock[
(define: x : Number 7)
(define: (id [z : Number]) : Number z)]

Here, @racket[x] has the type @racket[Number], and @racket[id] has the
type @racket[(-> Number Number)].  In the body of @racket[id],
@racket[z] has the type @racket[Number].

@subsection{Annotating Local Binding}

@racketblock[
(let: ([x : Number 7])
  (add1 x))
]

The @racket[let:] form is exactly like @racket[let], but type
annotations are provided for each variable bound.  Here, @racket[x] is
given the type @racket[Number].  The @racket[let*:] and
@racket[letrec:] are similar. Annotations are optional with
@racket[let:] and variants.

@racketblock[
(let-values: ([([x : Number] [y : String]) (values 7 "hello")])
  (+ x (string-length y)))
]

The @racket[let*-values:] and @racket[letrec-values:] forms are similar.

@subsection{Annotating Functions}

Function expressions also bind variables, which can be annotated with
types. This function expects two arguments, a @racket[Number] and a
@racket[String]:

@racketblock[(lambda: ([x : Number] [y : String]) (+ x 5))]

This function accepts at least one @racket[String], followed by
arbitrarily many @racket[Number]s.  In the body, @racket[y] is a list
of @racket[Number]s.

@racketblock[(lambda: ([x : String] (unsyntax @tt["."]) [y : Number #,**]) (apply + y))]

This function has the type @racket[(-> String Number #,** Number)].
Functions defined by cases may also be annotated:

@racketblock[(case-lambda: [() 0]
			   [([x : Number]) x])]

This function has the type
@racket[(case-> (-> Number) (-> Number Number))].

@subsection{Annotating Single Variables}

When a single variable binding needs annotation, the annotation can be
applied to a single variable using a reader extension:

@racketblock[
(let ([#,(annvar x Number) 7]) (add1 x))]

This is equivalent to the earlier use of @racket[let:]. This is
especially useful for binding forms which do not have counterparts
provided by Typed Racket, such as @racket[let+]:

@racketblock[
(let+ ([val #,(annvar x Number) (+ 6 1)])
  (* x x))]

@subsection{Annotating Expressions}

It is also possible to provide an expected type for a particular
expression.

@racketblock[(ann (+ 7 1) Number)]

This ensures that the expression, here @racket[(+ 7 1)], has the
desired type, here @racket[Number].  Otherwise, the type checker
signals an error.  For example:

@interaction[#:eval the-eval
(ann "not a number" Number)]

@section{Type Inference}

In many cases, type annotations can be avoided where Typed Racket can
infer them.  For example, the types of all local bindings using
@racket[let] and @racket[let*] can be inferred.

@racketblock[(let ([x 7]) (add1 x))]

In this example, @racket[x] has the type
@racket[Exact-Positive-Integer].

Similarly, top-level constant definitions do not require annotation:

@racketblock[(define y "foo")]

In this examples, @racket[y] has the type @racket[String].

Finally, the parameter types for loops are inferred from their initial
values.

@racketblock[
(let loop ([x 0] [y (list 1 2 3)])
  (if (null? y) x (loop (+ x (car y)) (cdr y))))]

Here @racket[x] has the inferred type @racket[Integer], and @racket[y]
has the inferred type @racket[(Listof Integer)].  The @racket[loop]
variable has type @racket[(-> Integer (Listof Integer) Integer)].

@subsection[#:tag "when-annotations?"]{When do you need type annotations?}

The last several subsections explained several ways to add type annotations
and explained that type inference allows some annotations to be left out.
Since annotations can often be omitted, it is helpful to know the situations
in which they are actually required.

The following four rules of thumb will usually suffice to determine
if a type annotation is necessary.

An expression or definition needs a type annotation if it:

@itemlist[
  @item{is a @racket[define] form for a function,}
  @item{is a @racket[lambda] that is immediately bound to a variable,}
  @item{is a @racket[lambda] that is an argument to a polymorphic function, or}
  @item{is defining a mutable variable.}
]

Here are examples that correspond to each of the cases above:

Example 1:
@racketblock[
  (: fn (-> String Symbol))
  (define (fn str) _...)
]
Example 2:
@racketblock[
  (: fn (-> String Symbol))
  (define fn (lambda (str) _...))
]
Example 3:
@racketblock[
  (map (lambda: ([n : Integer]) (add1 n)) '(1 2 3))
]
Example 4:
@racketblock[
   (: maybe-animal (Option String))
   (define maybe-animal #f)
   (set! maybe-animal "Odontodactylus scyllarus")
]

In all four cases, if the type annotation is omitted then the inferred type
will often be too conservative (e.g., @racket[Any]) and the code may not
type-check.

@section{New Type Names}

Any type can be given a name with @racket[define-type].

@racketblock[(define-type NN (-> Number Number))]

Anywhere the name @racket[NN] is used, it is expanded to
@racket[(-> Number Number)]. Type names may be recursive
or even mutually recursive.

@(close-eval the-eval)
