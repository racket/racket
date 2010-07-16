#lang scribble/manual

@begin[(require "utils.rkt" (for-label (only-meta-in 0 typed/racket)))]

@title[#:tag "top"]{@bold{Typed Racket}: Racket with Static Types}

@author["Sam Tobin-Hochstadt"]

@section-index["typechecking" "typechecker" "typecheck"]

Typed Racket is a family of languages, each of which enforce
that programs written in the language obey a type system that ensures
the absence of many common errors.  This guide is intended for programmers familiar 
with Racket.  For an introduction to Racket, see the @(other-manual '(lib "scribblings/guide/guide.scrbl")).

@local-table-of-contents[]

@include-section["quick.scrbl"]
@include-section["begin.scrbl"]
@include-section["more.scrbl"]
@include-section["types.scrbl"]
@include-section["optimization.scrbl"]

@;@section{How the Type System Works}

@;@section{Integrating with Untyped Code}
