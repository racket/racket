#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "bnf.ss" "scribble")]
@require["utils.ss"]

@title{PLT Scribble}

The @file{scribble} collection provides libraries that can be used to
create documents from Scheme.

@table-of-contents[]

@; ------------------------------------------------------------------------
@section{Scribble Layers}

Scribble is made of independently usable parts.  For example, the
Scribble reader can be used in any situation that requires lots of
free-form text. You can also skip Scribble's special reader support,
and instead use the document-generation structure directly.

The layers are:

@itemize{

 @item{@file{reader.ss}: a reader that extends the syntax of Scheme
       with @"@"-forms for conveniently embedding a mixin of text and
       escapes. See @secref["reader"].}

 @item{@file{struct.ss}: a set of document datatypes, which define the
       basic layout of a document. See @secref["struct"].}

 @item{@file{base-render.ss} with @file{html-render.ss},
       @file{latex-render.ss}, or @file{text-render.ss}: A base
       renderer and mixins that generate documents in various formats
       from instances of the @file{struct.ss} datatype. See
       @secref["renderer"].}

 @item{@file{decode.ss}: Processes a stream of text, section-start
       markers, etc. to produce instances of the @file{struct.ss}
       datatype. See @secref["decode"].}

 @item{@file{doclang.ss}: to be used for the initial import of a
       module; processes the module top level through
       @file{decode.ss}, and otherwise provides all of
       @scheme[mzscheme].  See @secref["doclang"].}

 @item{@file{docreader.ss}: a reader that is meant to tbe used to
       process an entire file; it essentially combines
       @file{reader.ss} with @file{doclang.ss}. See
       @secref["docreader"].}

 @item{@file{basic.ss}: a library of basic document operators---such
       as @scheme[title], @scheme[section], and @scheme[secref]---for
       use with @file{decode.ss} and a renderer. See
       @secref["basic"].}

 @item{@file{scheme.ss}: a library of support functions for
       typesetting Scheme code.}

 @item{@file{manual.ss}: a library of support functions for writing
       PLT Scheme documentation; re-exports @file{basic.ss}. See
       @secref["manual"].}

 @item{@file{eval.ss}: a library of support functions for ealuating
       code at document-build time, especially for showing
       examples. See @secref["eval"].}

 @item{@file{bnf.ss}: a library of support functions for writing
       grammars.}

}

The @exec{scribble} command-line utility works with a module that
exports a @scheme{struct.ss}-based document, generating output with a
specified renderer. More specifically, the executable installs a
renderer, loads the specified modules and extracts the @scheme[doc]
export of each (which must be an instance of @scheme[section] from
@file{struct.ss}), and renders each. Use @exec{scribble -h} for more
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
@include-section["style.scrbl"]
