#lang scribble/doc
@(require "common.rkt"
          (for-label syntax/stx
                     syntax/to-string))

@title[#:tag "to-string"]{Rendering Syntax Objects with Formatting}

@defmodule[syntax/to-string]

@defproc[(syntax->string [stx-list stx-list?]) string?]{

Builds a string with newlines and indenting according to the source
locations in @racket[stx-list]; the outer pair of parens are not
rendered from @racket[stx-list].}
