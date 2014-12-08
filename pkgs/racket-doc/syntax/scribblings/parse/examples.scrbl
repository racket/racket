#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title[#:tag "stxparse-examples" #:style '(toc)]{Examples}

This section provides an extended introduction to
@racketmodname[syntax/parse] as a series of worked examples.

@local-table-of-contents[]

@include-section["ex-mods-stxclasses.scrbl"]
@include-section["ex-kw-args.scrbl"]
@include-section["ex-uniform.scrbl"]
@include-section["ex-varied.scrbl"]
@include-section["ex-many-kws.scrbl"] @;{needs revision}
@include-section["ex-exprc.scrbl"]

@;{
@section{Communication via static bindings}
@section{Control: cut and commit}
@section{Analyzing expanded code}
}
