#lang scribble/doc
@(require "common.ss"
          (for-label syntax/context))

@title[#:tag "context"]{Support for @scheme[local-expand]}

@defmodule[syntax/context]

@defproc[(build-expand-context [v (or/c symbol? list?)]) list?]{

Returns a list suitable for use as a context argument to
@scheme[local-expand] for an internal-definition context. The
@scheme[v] argument represents the immediate context for
expansion. The context list builds on @scheme[(syntax-local-context)]
if it is a list.}

@defproc[(generate-expand-context) list?]{

Calls @scheme[build-expand-context] with a generated symbol.}
