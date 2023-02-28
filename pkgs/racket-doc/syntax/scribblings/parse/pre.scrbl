#lang scribble/doc
@(require scribble/manual
          "parse-common.rkt")

@title{Minimal Library}

@defmodule[syntax/parse/pre]

The @racketmodname[syntax/parse/pre] library is useful for accessing
most syntax-parsing functionality while minimizing library
dependencies. It provides most of @racketmodname[syntax/parse], but
omits these bindings:

@racketblock[
  expr/c
  pattern-expander?
  prop:syntax-class
  pattern-expander
  prop:pattern-expander
  syntax-local-syntax-parse-pattern-introduce
]

In addition, the provided variant of @racket[static] is a different
binding that lacks explicit contract checks.
