#lang scribble/manual
@(require "utils.rkt")

@title{Scribble as Preprocessor}

@author["Matthew Flatt" "Eli Barzilay"]

@section-index["Preprocessor"]

The @racketmodname[scribble/text] and @racketmodname[scribble/html]
languages act as ``preprocessor'' languages for generating text or
HTML. These preprocessor languages use the same @"@" syntax as the
main Scribble tool (see @other-doc['(lib
"scribblings/scribble/scribble.scrbl")]), but instead of working in
terms of a document abstraction that can be rendered to text and HTML
(and other formats), the preprocessor languages work in a way that is
more specific to the target formats.

@table-of-contents[]

@; ------------------------------------------------------------------------

@include-section["text.scrbl"]
@include-section["html.scrbl"]

@index-section[]
