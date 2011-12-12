#lang scribble/manual
@(require "common.rkt"
          scribble/eval
          (for-label racket/base 
                     syntax/datum))

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
@racket[clause] of @racket[datum-case] are accessible via
@racket[datum] instead of @racket[syntax]. When a @racket[literal-id]
appears in a @racket[clause]'s pattern, it matches the corresponding
symbol (using @racket[eq?]).


Using @racket[datum-case] and @racket[datum] is essentially equivalent
to converting the input to @racket[syntax-case] using
@racket[datum->syntax] and then wrapping each use of @racket[syntax]
with @racket[syntax->datum], but @racket[datum-case] and
@racket[datum] to not create intermediate syntax objects.

@examples[
#:eval datum-eval
(datum-case '(1 "x" -> y) (->)
  [(a ... -> b) (datum (b (+ a) ...))])
]}


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
