#lang scribble/doc
@(require "common.rkt" "diagrams.rkt")

@title[#:style '(toc quiet)]{Snip and Style Classes}

@declare-exporting[racket/snip racket/gui/base racket/gui]
@defmodule*/no-declare[(racket/snip)]

The @racketmodname[racket/snip] collection provides the core
@tech{snip} and @tech{style} classes @emph{without} depending on
@racketmodname[racket/gui/base]. This separation enables
libraries that can cooperate with an editor while also working in
contexts that do not have a GUI.

Snips and Administrators:

@diagram->table[snip-diagram]

Snip Lists:

@diagram->table[snip-list-diagram]

Styles:

@diagram->table[style-diagram]

Alphabetical:

@local-table-of-contents[]

@include-section["add-color-intf.scrbl"]
@include-section["image-snip-class.scrbl"]
@include-section["mult-color-intf.scrbl"]
@include-section["readable-snip-intf.scrbl"]
@include-section["snip-class.scrbl"]
@include-section["snip-admin-class.scrbl"]
@include-section["snip-class-class.scrbl"]
@include-section["snip-class-list-intf.scrbl"]
@include-section["string-snip-class.scrbl"]
@include-section["style-intf.scrbl"]
@include-section["style-delta-class.scrbl"]
@include-section["style-list-class.scrbl"]
@include-section["tab-snip-class.scrbl"]
