#lang scribble/doc
@(require "common.ss")

@title[#:style '(toc reveal)]{Overview}

For documentation purposes, the graphics toolbox is organized into
 three parts:

@itemize[

 @item{The @deftech{windowing toolbox}, for implementing form-filling
 GUI programs (such as a database query window) using buttons, menus,
 text fields, and events. The windowing toolbox is described in
 @secref["windowing-overview"].}

 @item{The @deftech{drawing toolbox}, for drawing pictures or
 implementing dynamic GUI programs (such as a video game) using
 drawing canvases, pens, and brushes. The drawing toolbox is
 described in @secref["drawing-overview"].}

 @item{The @deftech{editor toolbox}, for developing traditional text
 editors, editors that mix text and graphics, or free-form layout
 editors (such as a word processor, HTML editor, or icon-based file
 browser).  The editor toolbox is described in
 @secref["editor-overview"].}

]

These three parts roughly represent layers of increasing
 sophistication. Simple GUI programs access only the windowing toolbox
 directly, more complex programs use both the windowing and drawing
 toolboxes, and large-scale applications rely on all three
 toolboxes.

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["win-overview.scrbl"]

@;------------------------------------------------------------------------

@include-section["draw-overview.scrbl"]

@;------------------------------------------------------------------------

@include-section["editor-overview.scrbl"]
