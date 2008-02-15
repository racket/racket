#lang scribble/doc
@(require "common.ss"
          (for-label syntax/toplevel))

@title[#:tag "toplevel"]{Non-Module Compilation And Expansion}

@defmodule[syntax/toplevel]

@defproc[(expand-syntax-top-level-with-compile-time-evals [stx syntax?])
         syntax?]{

Expands @scheme[stx] as a top-level expression, and evaluates its
compile-time portion for the benefit of later expansions.

The expander recognizes top-level @scheme[begin] expressions, and
interleaves the evaluation and expansion of of the @scheme[begin]
body, so that compile-time expressions within the @scheme[begin] body
affect later expansions within the body. (In other words, it ensures
that expanding a @scheme[begin] is the same as expanding separate
top-level expressions.)

The @scheme[stx] should have a context already, possibly introduced with
@scheme[namespace-syntax-introduce].}

@defproc[(expand-top-level-with-compile-time-evals [stx syntax?]) 
         syntax?]{

Like @scheme[expand-syntax-top-level-with-compile-time-evals], but
@scheme[stx] is first given context by applying
@scheme[namespace-syntax-introduce] to it.}

@defproc[(expand-syntax-top-level-with-compile-time-evals/flatten [stx syntax?])
         (listof syntax?)]{

Like @scheme[expand-syntax-top-level-with-compile-time-evals], except
that it returns a list of syntax objects, none of which have a
@scheme[begin]. These syntax objects are the flattened out contents of
any @scheme[begin]s in the expansion of @scheme[stx].}

@defproc[(eval-compile-time-part-of-top-level [stx syntax?]) 
         void?]{

Evaluates expansion-time code in the fully expanded top-level
expression represented by @scheme[stx] (or a part of it, in the case
of @scheme[begin] expressions). The expansion-time code might affect
the compilation of later top-level expressions. For example, if
@scheme[stx] is a @scheme[require] expression, then
@scheme[namespace-require/expansion-time] is used on each require
specification in the form. Normally, this function is used only by
@scheme[expand-top-level-with-compile-time-evals].}

@defproc[(eval-compile-time-part-of-top-level/compile [stx syntax?])
	 (listof compiled-expression?)]{ 

Like @scheme[eval-compile-time-part-of-top-level], but the result is
compiled code.}
