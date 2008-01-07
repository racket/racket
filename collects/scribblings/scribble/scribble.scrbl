#lang scribble/doc
@require[scribble/manual]
@require[scribble/bnf]
@require["utils.ss"]

@title[#:tag-prefix '(lib "scribblings/scribble/scribble.scrbl") 
       #:tag "top"]{@bold{Scribble}: PLT Documentation Tool}

Scribble is a collection of tools for creating prose documents,
especially those that document libraries.

@table-of-contents[]

@; ------------------------------------------------------------------------
@include-section["how-to.scrbl"]
@include-section["layers.scrbl"]
@include-section["reader.scrbl"]
@include-section["struct.scrbl"]
@include-section["renderer.scrbl"]
@include-section["decode.scrbl"]
@include-section["doclang.scrbl"]
@include-section["docreader.scrbl"]
@include-section["basic.scrbl"]
@include-section["scheme.scrbl"]
@include-section["manual.scrbl"]
@include-section["eval.scrbl"]
@include-section["bnf.scrbl"]
@include-section["xref.scrbl"]

@index-section[]
