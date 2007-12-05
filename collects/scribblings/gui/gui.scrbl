#lang scribble/doc
@require["common.ss"]

@title[#:tag-prefix '(lib "scribblings/gui/gui.scrbl") 
       #:tag "top"]{PLT Scheme GUI: MrEd}

@declare-exporting[mred scheme/gui]

This reference manual describes the MrEd GUI toolbox that is part of
 PLT Scheme. See @secref[#:doc '(lib "scribblings/guide/guide.scrbl")
 "mred"] in @italic{@link["../guide/index.html"]{A Guide to PLT
 Scheme}} for an introduction to MrEd.

@defmodule*/no-declare[(mred)]{The @schememodname[mred] module provides
all of the class, interface, and procedure bindings defined in this
manual.}

@defmodulelang*/no-declare[(scheme/gui)]{The
@schememodname[scheme/gui] language combines all bindings of the
@schememodname[scheme] language and the @schememodname[mred] module.}


@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@include-section["config.scrbl"]

@;------------------------------------------------------------------------

@index-section[]
