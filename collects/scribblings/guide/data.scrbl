#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "datatypes" #:style 'toc]{Built-In Datatypes}

The @seclink["to-scheme"]{previous chapter} introduced some of
Racket's built-in datatypes: numbers, booleans, strings, lists, and
procedures. This section provides a more complete coverage of the
built-in datatypes for simple forms of data.

@local-table-of-contents[]

@include-section["booleans.scrbl"]
@include-section["numbers.scrbl"]
@include-section["chars.scrbl"]
@include-section["char-strings.scrbl"]
@include-section["byte-strings.scrbl"]
@include-section["symbols.scrbl"]
@include-section["keywords.scrbl"]
@include-section["pairs.scrbl"]
@include-section["vectors.scrbl"]
@include-section["hash-tables.scrbl"]
@include-section["boxes.scrbl"]
@include-section["void-and-undef.scrbl"]

@; @include-section["paths.scrbl"]
@; @include-section["regexps-data.scrbl"]
