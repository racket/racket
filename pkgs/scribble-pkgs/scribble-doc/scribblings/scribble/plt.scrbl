#lang scribble/manual
@(require "utils.rkt")

@title[#:tag "plt-manuals" #:style 'toc]{Scribbling Documentation}

The @racketmodname[scribble/manual] language and associated libraries
provide extensive support for documenting Racket libraries. The
most significant aspect of support for documentation is the way that
source-code bindings are connected to documentation sites through the
module namespace---a connection that is facilitated by the fact that
Scribble documents are themselves modules that reside in the same
namespace. @Secref["how-to-doc"] provides an introduction to using
Scribble for documentation, and the remaining sections document the
relevant libraries and APIs in detail.

@local-table-of-contents[]

@include-section["how-to.scrbl"]
@include-section["style.scrbl"]
@include-section["manual.scrbl"]
@include-section["scheme.scrbl"]
@include-section["eval.scrbl"]
@include-section["srcdoc.scrbl"]
@include-section["bnf.scrbl"]
@include-section["compat.scrbl"]
