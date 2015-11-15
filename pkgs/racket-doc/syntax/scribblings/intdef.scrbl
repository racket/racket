#lang scribble/doc
@(require "common.rkt" (for-label syntax/intdef))

@title[#:tag "intdef"]{Internal-Definition Context Helpers}

@defmodule[syntax/intdef]

@history[#:added "6.3.0.4"]

@defproc[(internal-definition-context-track
          [intdef-ctx internal-definition-context?]
          [stx syntax?])
         syntax?]{

Adjusts the @tech[#:doc refman]{syntax properties} of @racket[stx] to
record that parts of @racket[stx] were expanded via
@racket[intdef-ctx].

Specifically, the identifiers produced by
@racket[(internal-definition-context-binding-identifiers intdef-ctx)]
are added to the @racket['disappeared-bindings] property of
@racket[stx].}
