#lang scribble/manual

@begin[(require "../utils.rkt"
		scribble/core scribble/eval
		(for-label (only-meta-in 0 typed/racket)))]

@(define the-eval (make-base-eval))
@(the-eval '(require typed/racket))

@title[#:tag "occurrence-typing"]{Occurrence Typing}

@section{Basic Occurrence Typing}

One of Typed Racket's distinguishing type system features is
@deftech{occurrence typing}, which allows the type system to ascribe
more precise types based on whether a predicate check succeeds or
fails.

To illustrate, consider the following code:

@racketblock+eval[#:eval the-eval
  (: flexible-length (-> (U String (Listof Any)) Integer))
  (define (flexible-length str-or-lst)
    (if (string? str-or-lst)
        (string-length str-or-lst)
        (length str-or-lst)))
]

The @racket[_flexible-length] function above computes the length of
either a string or a list. The function body uses the typical Racket
idiom of dispatching using a predicate (e.g., @racket[string?]).

Typed Racket successfully type-checks this function because the type
system understands that in the "then" branch of the @racket[if]
expression, the predicate @racket[string?] must have returned a true
value.  The type system further knows that if @racket[string?]
returns true, then the @racket[_str-or-lst] variable must have type
@racket[String] and can narrow the type from its original union of
@racket[String] and @racket[(Listof Any)]. This allows the call to
@racket[string-length] in the "then" branch to type-check
successfully.

Furthermore, the type system also knows that in the "else" branch of
the @racket[if] expression, the predicate must have returned
@racket[#f].  This implies that the variable @racket[_str-or-lst] must
have type @racket[(Listof Any)] by process of elimination, and thus
the call @racket[(length str-or-lst)] type-checks.

To summarize, if Typed Racket can determine the type a variable must
have based on a predicate check in a conditional expression, it can
narrow the type of the variable within the appropriate branch of the
conditional.

@section[#:tag "filters-and-predicates"]{Filters and Predicates}

In the previous section, we demonstrated that a Typed Racket programmer
can take advantage of occurrence typing to type-check functions
with union types and conditionals. This may raise the question: how
does Typed Racket know how to narrow the type based on the predicate?

The answer is that predicate types in Typed Racket are annotated
with @deftech{filters} that tell the typechecker what additional
information is gained when a predicate check succeeds or fails.

For example, consider the REPL's type printout for @racket[string?]:

@interaction[#:eval the-eval string?]

The type @racket[(-> Any Boolean : String)] has three parts. The first
two are the same as any other function type and indicate that the
predicate takes any value and returns a boolean. The third part, after
the @racket[_:], is a @tech{filter} that tells the typechecker two
things:

@itemlist[#:style 'ordered
  @item{If the predicate check succeeds, the argument variable has type @racket[String]}
  @item{If the predicate check fails, the argument variable @emph{does not} have type @racket[String]}
]

Predicates for all built-in types are annotated with similar filters
that allow the type system to reason about predicate checks.

@section{Other conditionals and assertions}

@margin-note{After all, these control flow constructs macro-expand
  to @racket[if] in the end.}

So far, we have seen that occurrence typing allows precise reasoning
about @racket[if] expressions. Occurrence typing works for most
control flow constructs that are present in Racket such as
@racket[cond], @racket[when], and others.

For example, the @racket[_flexible-length] function from earlier can
be re-written to use @racket[cond] with no additional effort:

@racketblock+eval[#:eval the-eval
  (: flexible-length/cond (-> (U String (Listof Any)) Integer))
  (define (flexible-length/cond str-or-lst)
    (cond [(string? str-or-lst) (string-length str-or-lst)]
          [else (length str-or-lst)]))
]

In some cases, the type system does not have enough information or is
too conservative to typecheck an expression. For example, consider
the following interaction:

@interaction[#:eval the-eval
(: a Positive-Integer)
(define a 15)
(: b Positive-Integer)
(define b 20)
(: c Positive-Integer)
(define c (- b a))
]

In this case, the type system only knows that @racket[_a] and
@racket[_b] are positive integers and cannot conclude that their
difference will always be positive in defining @racket[_c].  In cases
like this, occurrence typing can be used to make the code type-check
using an @emph{assertion}. For example,

@racketblock+eval[#:eval the-eval
(: d Positive-Integer)
(define d (assert (- b a) positive?))
]

Using the filter on @racket[positive?], Typed Racket can assign the
type @racket[Positive-Integer] to the whole @racket[assert]
expression.  This type-checks, but note that the assertion may raise
an exception at run-time if the predicate returns @racket[#f].

Note that @racket[assert] is a derived concept in Typed Racket and is
a natural consequence of occurrence typing. The assertion above is
essentially equivalent to the following:

@racketblock+eval[#:eval the-eval
(: e Positive-Integer)
(define e (let ([diff (- b a)])
            (if (positive? diff)
                diff
                (error "Assertion failed"))))
]

@section{A caveat about @racket[set!]}

If a variable is ever mutated with @racket[set!]  in the scope in
which it is defined, Typed Racket cannot use occurrence typing with
that variable. This precaution is needed to ensure that concurrent
modification of a variable does not invalidate Typed Racket's
knowledge of the type of that variable.  Also see
@Secref["using-set!"  #:doc '(lib "scribblings/guide/guide.scrbl")].

Furthermore, this means that the types of @rtech{top-level variable}s
in the @gtech{REPL} cannot be refined by Typed Racket either. This is
because the scope of a top-level variable includes future top-level
interactions, which may include mutations. It is possible to work
around this by moving the variable inside of a module or into a local
binding form like @racket[let].

