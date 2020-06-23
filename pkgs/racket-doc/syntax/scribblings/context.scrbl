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

@defproc[(generate-expand-context [liberal-definitions? boolean? #f]) list?]{

Calls @racket[build-expand-context] with a generated unique value.
When @racket[liberal-definitions?] is true, the value is an instance of
a structure type with a true value for the @racket[prop:liberal-define-context]
property.

}
