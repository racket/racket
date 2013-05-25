#lang scribble/doc
@(require "common.rkt")

@title{The Racket Graphical Interface Toolkit}

@author["Matthew Flatt" "Robert Bruce Findler" "John Clements"]

@declare-exporting[racket/gui/base racket/gui #:use-sources (mred)]

@defmodule*/no-declare[(racket/gui/base)]{The
@racketmodname[racket/gui/base] library provides all of the class,
interface, and procedure bindings defined in this manual, in addition
to the bindings of @racketmodname[racket/draw] and 
@racketmodname[file/resource].}

@defmodulelang*/no-declare[(racket/gui)]{The
@racketmodname[racket/gui] language combines all bindings of the
@racketmodname[racket] language and the
@racketmodname[racket/gui/base] and @racketmodname[racket/draw] modules.}

The @racketmodname[racket/gui] toolbox is roughly organized into two
parts:

@itemize[

 @item{The @deftech{windowing toolbox}, for implementing windows,
 buttons, menus, text fields, and other controls.}

 @item{The @deftech{editor toolbox}, for developing traditional text
 editors, editors that mix text and graphics, or free-form layout
 editors (such as a word processor, HTML editor, or icon-based file
 browser).}

]

Both parts of the toolbox rely extensively on the
@racketmodname[racket/draw] drawing library.

@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["win-overview.scrbl"]
@include-section["widget-gallery.scrbl"]
@include-section["win-classes.scrbl"]
@include-section["win-funcs.scrbl"]
@include-section["editor-overview.scrbl"]
@include-section["snip-classes.scrbl"]
@include-section["editor-classes.scrbl"]
@include-section["editor-funcs.scrbl"]
@include-section["wxme.scrbl"]
@include-section["prefs.scrbl"]
@include-section["dynamic.scrbl"]
@include-section["startup.scrbl"]
@include-section["libs.scrbl"]

@;------------------------------------------------------------------------

@index-section[]
