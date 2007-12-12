#lang scribble/doc
@require["common.ss"]

@title[#:tag-prefix '(lib "scribblings/gui/gui.scrbl") 
       #:tag "top"]{@bold{GUI}: PLT Graphics Toolkit}

@declare-exporting[scheme/gui/base scheme/gui]

This reference manual describes the MrEd GUI toolbox that is part of
 PLT Scheme. See @secref[#:doc '(lib "scribblings/guide/guide.scrbl")
 "mred"] in @italic{@link["../guide/index.html"]{A Guide to PLT
 Scheme}} for an introduction to MrEd.

@defmodule*/no-declare[(scheme/gui/base)]{The
@schememodname[scheme/gui/base] module provides all of the class,
interface, and procedure bindings defined in this manual.}

@defmodulelang*/no-declare[(scheme/gui)]{The
@schememodname[scheme/gui] language combines all bindings of the
@schememodname[scheme] language and the
@schememodname[scheme/gui/base] modules.}


@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@include-section["config.scrbl"]
@include-section["dynamic.scrbl"]

@;------------------------------------------------------------------------

@index-section[]
