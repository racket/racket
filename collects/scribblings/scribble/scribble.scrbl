#lang scribble/doc
@require[scribble/manual]
@require[scribble/bnf]
@require["utils.ss"]

@title[#:tag-prefix '(lib "scribblings/scribble/scribble.scrbl") 
       #:tag "top"]{Scribble}

The @filepath{scribble} collection provides libraries that can be used to
create documents from Scheme.

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

 @item{@filepath{reader.ss}: a reader that extends the syntax of Scheme
       with @"@"-forms for conveniently embedding a mixin of text and
       escapes. See @secref["reader"].}

 @item{@filepath{struct.ss}: a set of document datatypes and utilities
       that define the basic layout and processing of a document. See
       @secref["struct"].}

 @item{@filepath{base-render.ss} with @filepath{html-render.ss},
       @filepath{latex-render.ss}, or @filepath{text-render.ss}: A base
       renderer and mixins that generate documents in various formats
       from instances of the @filepath{struct.ss} datatypes. See
       @secref["renderer"].}

 @item{@filepath{decode.ss}: Processes a stream of text, section-start
       markers, etc. to produce instances of the @filepath{struct.ss}
       datatypes. See @secref["decode"].}

 @item{@filepath{doclang.ss}: to be used for the initial import of a
       module; processes the module top level through
       @filepath{decode.ss}, and otherwise provides all of
       @schememodname[big].  See @secref["doclang"].}

 @item{@filepath{docreader.ss}: a reader that is meant to tbe used to
       process an entire file; it essentially combines
       @filepath{reader.ss} with @filepath{doclang.ss}. See
       @secref["docreader"].}

 @item{@filepath{basic.ss}: a library of basic document operators---such
       as @scheme[title], @scheme[section], and @scheme[secref]---for
       use with @filepath{decode.ss} and a renderer. See
       @secref["basic"].}

 @item{@filepath{scheme.ss}: a library of support functions for
       typesetting Scheme code.}

 @item{@filepath{manual.ss}: a library of support functions for writing
       PLT Scheme documentation; re-exports @filepath{basic.ss}. See
       @secref["manual"].}

 @item{@filepath{eval.ss}: a library of support functions for ealuating
       code at document-build time, especially for showing
       examples. See @secref["eval"].}

 @item{@filepath{bnf.ss}: a library of support functions for writing
       grammars.}

}

The @exec{scribble} command-line utility works with a module that
exports a @scheme{struct.ss}-based document, generating output with a
specified renderer. More specifically, the executable installs a
renderer, loads the specified modules and extracts the @scheme[doc]
export of each (which must be an instance of @scheme[section] from
@filepath{struct.ss}), and renders each. Use @exec{scribble -h} for more
information.

@; ------------------------------------------------------------------------
@include-section["reader.scrbl"]
@include-section["struct.scrbl"]
@include-section["renderer.scrbl"]
@include-section["decode.scrbl"]
@include-section["doclang.scrbl"]
@include-section["docreader.scrbl"]
@include-section["basic.scrbl"]
@include-section["manual.scrbl"]
@include-section["eval.scrbl"]

@index-section["scribble-index"]
