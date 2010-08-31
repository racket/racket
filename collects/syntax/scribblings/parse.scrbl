#lang scribble/doc
@(require scribble/manual
          (for-label syntax/parse))

@title[#:tag "stxparse" #:style '(toc)]{Parsing and classifying syntax}

The @schememodname[syntax/parse] library provides a framework for
writing macros and processing syntax. The library provides a powerful
language of syntax patterns, used by the pattern-matching form
@scheme[syntax-parse] and the specification form
@scheme[define-syntax-class]. Macros that use @scheme[syntax-parse]
automatically generate error messages based on descriptions and
messages embedded in the macro's syntax patterns.
@defmodule[syntax/parse]

@local-table-of-contents[]

@include-section["parse/intro.scrbl"]
@include-section["parse/examples.scrbl"]
@include-section["parse/parsing.scrbl"]
@include-section["parse/patterns.scrbl"]
@include-section["parse/litconv.scrbl"]
@include-section["parse/lib.scrbl"]

@;{Description of how error reporting works}
@;{and designing for good errors}

@;{Cut and Commit for efficiency and error reporting.}

@include-section["parse/debug.scrbl"]
@include-section["parse/experimental.scrbl"]
