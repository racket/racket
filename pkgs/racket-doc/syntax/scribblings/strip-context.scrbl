#lang scribble/doc
@(require "common.rkt" (for-label syntax/strip-context))

@title[#:tag "strip-context"]{Replacing Lexical Context}

@defmodule[syntax/strip-context]

@defproc[(strip-context [form any/c]) any/c]{

Removes all lexical context from syntax objects within @racket[form], preserving
source-location information and properties.

Typically, @racket[form] is a syntax object, and then the result is
also a syntax object. Otherwise, pairs, vectors, boxes, hash tables,
and prefab structures are traversed (and copied for the result) to
find syntax objects. Graph structure is not preserved in the result,
and cyclic data structures will cause @racket[strip-context] to never
return.

@history[#:changed "7.7.0.10" @elem{Repaired to traverse hash tables in @racket[stx].}]}


@defproc[(replace-context [ctx-stx (or/c syntax? #f)] [form any/c]) any/c]{

Uses the lexical context of @racket[ctx-stx] to replace the lexical
context of all parts of all syntax objects in @racket[form], preserving source-location
information and properties of those syntax objects.

Syntax objects are found in @racket[form] the same as in
@racket[strip-context].

@history[#:changed "7.7.0.10" @elem{Repaired to traverse hash tables in @racket[stx].}]}
