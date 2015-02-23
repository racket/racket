#lang scribble/doc
@(require "common.rkt" (for-label syntax/context))

@title[#:tag "context"]{Support for @racket[local-expand]}

@defmodule[syntax/context]

@defproc[(build-expand-context [v (or/c symbol? list?)]) list?]{

Returns a list suitable for use as a context argument to
@racket[local-expand] for an internal-definition context. The
@racket[v] argument represents the immediate context for
expansion. The context list builds on @racket[(syntax-local-context)]
if it is a list.}

@defproc[(generate-expand-context) list?]{

Calls @racket[build-expand-context] with a generated symbol.}
