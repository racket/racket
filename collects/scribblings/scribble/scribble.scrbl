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

@; ------------------------------------------------------------------------
@section{Scribble Layers}

Scribble is made of independently usable parts.  For example, the
Scribble reader can be used in any situation that requires lots of
free-form text. You can also skip Scribble's special reader support,
and instead use the document-generation structure directly.

The layers are:

@itemize{

 @item{@schememodname[scribble/reader]: a reader that extends the
       syntax of Scheme with @"@"-forms for conveniently embedding a
       mixin of text and escapes. See @secref["reader"].}

 @item{@schememodname[scribble/struct]: a set of document datatypes and utilities
       that define the basic layout and processing of a document. See
       @secref["struct"].}

 @item{@schememodname[scribble/base-render] with @schememodname[scribble/html-render],
       @schememodname[scribble/latex-render], or @schememodname[scribble/text-render]: A base
       renderer and mixins that generate documents in various formats
       from instances of the @schememodname[scribble/struct] datatypes. See
       @secref["renderer"].}

 @item{@schememodname[scribble/decode]: Processes a stream of text, section-start
       markers, etc. to produce instances of the @schememodname[scribble/struct]
       datatypes. See @secref["decode"].}

 @item{@schememodname[scribble/doclang]: to be used for the initial import of a
       module; processes the module top level through
       @schememodname[scribble/decode], and otherwise provides all of
       @schememodname[scheme/base].  See @secref["doclang"].}

 @item{@schememodname[scribble/doc]: a language that essentially
       combines @schememodname[scribble/reader] with
       @schememodname[scribble/doclang]. See @secref["docreader"].}

 @item{@schememodname[scribble/basic]: a library of basic document operators---such
       as @scheme[title], @scheme[section], and @scheme[secref]---for
       use with @schememodname[scribble/decode] and a renderer. See
       @secref["basic"].}

 @item{@schememodname[scribble/scheme]: a library of support functions for
       typesetting Scheme code. See @secref["scheme"].}

 @item{@schememodname[scribble/manual]: a library of support functions
       for writing PLT Scheme documentation; re-exports
       @schememodname[scribble/basic]. Also, the
       @schememodname[scribble/manual-struct] library provides
       types for index-entry descriptions created by functions in
       @schememodname[scribble/manual]. See @secref["manual"].}

 @item{@schememodname[scribble/eval]: a library of support functions
       for evaluating code at document-build time, especially for
       showing examples. See @secref["eval"].}

 @item{@schememodname[scribble/bnf]: a library of support functions for writing
       grammars. See @secref["bnf"].}

 @item{@schememodname[scribble/xref]: a library of support functions
       for using cross-reference information, typically after a
       document is rendered (e.g., to search). See @secref["xref"].}

}

The @exec{scribble} command-line utility generates output with a
specified renderer. More specifically, the executable installs a
renderer, loads the modules specified on the command line, extracts
the @scheme[doc] export of each module (which must be an instance of
@scheme[part]), and renders each. Use @exec{scribble -h} for more
information.

@; ------------------------------------------------------------------------
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
