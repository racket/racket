#lang scribble/manual
@(require "common.rkt"
          scribble/eval
          (for-label racket/base 
                     syntax/datum
                     syntax/parse))

@(define datum-eval (make-base-eval))
@interaction-eval[#:eval datum-eval (require syntax/datum)]

@title{Datum Pattern Matching}

@defmodule[syntax/datum]{The @racketmodname[syntax/datum] library
provides forms that implement the pattern and template language of
@racket[syntax-case], but for matching and constructing datum values
instead of syntax.}

For most pattern-matching purposes, @racketmodname[racket/match] is a
better choice than @racketmodname[syntax/datum]. The
@racketmodname[syntax/datum] library is useful mainly for its template
support (i.e., @racket[datum]) and, to a lesser extent, its direct
correspondence to @racket[syntax-case] patterns.


@deftogether[(
@defform[(datum-case datum-expr (literal-id ...)
           clause ...)]
@defform[(datum template)]
)]{

Like @racket[syntax-case] and @racket[syntax], but @racket[datum-expr]
in @racket[datum-case] should produce a @tech[#:doc refman]{datum}
(i.e., plain S-expression) instead of a @tech[#:doc refman]{syntax
object} to be matched in @racket[clause]s, and @racket[datum]
similarly produces a datum.  Pattern variables bound in each
@racket[clause] of @racket[datum-case] (or @racket[syntax-case], see
below) are accessible via @racket[datum] instead of
@racket[syntax]. When a @racket[literal-id] appears in a
@racket[clause]'s pattern, it matches the corresponding symbol (using
@racket[eq?]).

Using @racket[datum-case] and @racket[datum] is similar
to converting the input to @racket[syntax-case] using
@racket[datum->syntax] and then wrapping each use of @racket[syntax]
with @racket[syntax->datum], but @racket[datum-case] and
@racket[datum] do not create intermediate syntax objects, and they do
not destroy existing syntax objects within the S-expression structure
of @racket[datum-expr].

@examples[
#:eval datum-eval
(datum-case '(1 "x" -> y) (->)
  [(a ... -> b) (datum (b (+ a) ...))])
]

The @racket[datum] form also cooperates with @tech[#:key "pattern
variable" #:doc '(lib "scribblings/reference/reference.scrbl")]{syntax
pattern variables} such as those bound by @racket[syntax-case] and
@tech{attributes} bound by @racket[syntax-parse] (see
@secref["stxparse-attrs"] for more information about attributes). As
one consequence, @racket[datum] provides a convenient way of getting
the list of syntax objects bound to a syntax pattern variable of depth
1. For example, the following expressions are equivalent, except that
the @racket[datum] version avoids creating and eliminating a
superfluous syntax object wrapper:

@interaction[#:eval datum-eval
(with-syntax ([(x ...) #'(a b c)])
  (syntax->list #'(x ...)))
(with-syntax ([(x ...) #'(a b c)])
  (datum (x ...)))
]

A template can also use multiple syntax or datum pattern variables and
datum constants, and it can use the @racket[~@] and @racket[~?]
template forms:

@interaction[#:eval datum-eval
(with-syntax ([(x ...) #'(a b c)])
  (with-datum ([(y ...) (list 1 2 3)])
    (datum ([x -> y] ...))))
(with-syntax ([(x ...) #'(a b c)])
  (with-datum ([(y ...) (list 1 2 3)])
    (datum ((~@ x y) ...))))
]

See @secref["attributes-and-datum"] for examples of @racket[~?] with
@racket[datum].

If a datum variable is used in a syntax template, a compile-time error
is raised.

@history[#:changed "7.8.0.9" @elem{Changed @racket[datum] to
cooperate with @racket[syntax-case], @racket[syntax-parse], etc.}]}


@defform[(with-datum ([pattern datum-expr] ...)
           body ...+)]{

Analogous to @racket[with-syntax], but for @racket[datum-case] and
@racket[datum] instead of @racket[syntax-case] and @racket[syntax].

@examples[
#:eval datum-eval
(with-datum ([(a ...) '(1 2 3)]
             [(b ...) '("x" "y" "z")])
  (datum ((a b) ...)))
]}


@deftogether[(
@defform[(quasidatum template)]
@defform[(undatum expr)]
@defform[(undatum-splicing expr)]
)]{

Analogous to @racket[quasisyntax], @racket[unsyntax], and
@racket[unsyntax-splicing].

@examples[
#:eval datum-eval
(with-datum ([(a ...) '(1 2 3)])
  (quasidatum ((undatum (- 1 1)) a ... (undatum (+ 2 2)))))
]}


@close-eval[datum-eval]
