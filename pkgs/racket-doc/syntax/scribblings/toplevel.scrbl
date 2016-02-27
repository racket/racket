#lang scribble/doc
@(require "common.rkt" (for-label syntax/toplevel))

@title[#:tag "toplevel"]{Non-Module Compilation And Expansion}

@defmodule[syntax/toplevel]

@defproc[(expand-syntax-top-level-with-compile-time-evals [stx syntax?])
         syntax?]{

Expands @racket[stx] as a top-level expression, and evaluates its
compile-time portion for the benefit of later expansions.

The expander recognizes top-level @racket[begin] expressions, and
interleaves the evaluation and expansion of the @racket[begin]
body, so that compile-time expressions within the @racket[begin] body
affect later expansions within the body. (In other words, it ensures
that expanding a @racket[begin] is the same as expanding separate
top-level expressions.)

The @racket[stx] should have a context already, possibly introduced with
@racket[namespace-syntax-introduce].}

@defproc[(expand-top-level-with-compile-time-evals [stx syntax?]) 
         syntax?]{

Like @racket[expand-syntax-top-level-with-compile-time-evals], but
@racket[stx] is first given context by applying
@racket[namespace-syntax-introduce] to it.}

@defproc[(expand-syntax-top-level-with-compile-time-evals/flatten [stx syntax?])
         (listof syntax?)]{

Like @racket[expand-syntax-top-level-with-compile-time-evals], except
that it returns a list of syntax objects, none of which have a
@racket[begin]. These syntax objects are the flattened out contents of
any @racket[begin]s in the expansion of @racket[stx].}

@defproc[(eval-compile-time-part-of-top-level [stx syntax?]) 
         void?]{

Evaluates expansion-time code in the fully expanded top-level
expression represented by @racket[stx] (or a part of it, in the case
of @racket[begin] expressions). The expansion-time code might affect
the compilation of later top-level expressions. For example, if
@racket[stx] is a @racket[require] expression, then
@racket[namespace-require/expansion-time] is used on each require
specification in the form. Normally, this function is used only by
@racket[expand-top-level-with-compile-time-evals].}

@defproc[(eval-compile-time-part-of-top-level/compile [stx syntax?])
	 (listof compiled-expression?)]{ 

Like @racket[eval-compile-time-part-of-top-level], but the result is
compiled code.}
