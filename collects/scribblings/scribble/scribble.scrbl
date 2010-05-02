#lang scribble/manual
@(require scribble/bnf
          "utils.ss")

@title{@bold{Scribble}: Racket Documentation Tool}

@author["Matthew Flatt" "Eli Barzilay"]

Scribble is a collection of tools for creating prose
documents---papers, books, library documentation, etc.---in HTML or
PDF (via Latex) form. More generally, Scribble helps you write
programs that are rich in textual content, whether the content is
prose to be typeset or some other form of text to be generated
programmatically.

This document itself is written using Scribble. At the time that it
was written, its source was available at
@(let ([url "http://git.racket-lang.org/plt/tree/HEAD:/collects/scribblings/scribble"])
   (link url url))
starting with the @filepath{scribble.scrbl} file.

@table-of-contents[]

@; ------------------------------------------------------------------------
@include-section["how-to-paper.scrbl"]
@include-section["reader.scrbl"]
@include-section["generic.scrbl"]
@include-section["plt.scrbl"]
@include-section["lp.scrbl"]
@include-section["preprocessor.scrbl"]
@include-section["internals.scrbl"]

@index-section[]
