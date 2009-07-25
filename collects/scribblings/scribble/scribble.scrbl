#lang scribble/manual
@(require scribble/bnf
          "utils.ss")

@title{@bold{Scribble}: PLT Documentation Tools}

@author["Matthew Flatt" "Eli Barzilay"]

Scribble is a collection of tools for creating ASCII, HTML, and
Latex/PDF documents with PLT Scheme. Suitable uses include the
creation of papers, books, literate programs, preprocessed text, and
PLT Scheme library documentation.

This document itself is written using Scribble. At the time that it
was written, its source was available at
@(let ([url "http://svn.plt-scheme.org/plt/trunk/collects/scribblings/scribble/"])
   (link url url))
starting with the @filepath{scribble.scrbl} file.

@table-of-contents[]

@; ------------------------------------------------------------------------
@include-section["getting-started.scrbl"]
@include-section["generic.scrbl"]
@include-section["plt.scrbl"]
@include-section["lp.scrbl"]
@include-section["preprocessor.scrbl"]
@include-section["internals.scrbl"]

@index-section[]
