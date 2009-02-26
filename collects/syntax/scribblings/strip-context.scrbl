#lang scribble/doc
@(require "common.ss"
          (for-label syntax/strip-context))

@title[#:tag "strip-context"]{Stripping Lexical Context}

@defmodule[syntax/strip-context]

@defproc[(strip-context [stx syntax?]) syntax?]{

Removes all lexical context from @scheme[stx], preserving
source-location information and properties.}
