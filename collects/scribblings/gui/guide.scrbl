#lang scribble/doc
@(require "common.ss")

@title[#:style '(toc reveal)]{Overview}

For documentation purposes, the graphics toolbox is organized into
 two parts:

@itemize[

 @item{The @deftech{windowing toolbox}, for implementing form-filling
 GUI programs (such as a database query window) using buttons, menus,
 text fields, and events. The windowing toolbox is described in
 @secref["windowing-overview"].}

 @item{The @deftech{editor toolbox}, for developing traditional text
 editors, editors that mix text and graphics, or free-form layout
 editors (such as a word processor, HTML editor, or icon-based file
 browser).  The editor toolbox is described in
 @secref["editor-overview"].}

]

Simple GUI programs access only the windowing toolbox directly, while
 large-scale applications tend to use the editor toolbox as well.

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["win-overview.scrbl"]

@;------------------------------------------------------------------------

@include-section["editor-overview.scrbl"]
