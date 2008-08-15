#lang scribble/doc
@(require "common.ss")

@title{@bold{MysterX}: Using Windows COM Objects in Scheme}

@author["Paul Steckler"]

@bold{MysterX} is a toolkit for building Windows applications from
@as-index{ActiveX} and COM components, using Scheme as glue code.
Dynamic HTML (DHTML) is used for component presentation and
event-handling.

@defmodule[mysterx]

@table-of-contents[]

@include-section["overview.scrbl"]
@include-section["com.scrbl"]
@include-section["activex.scrbl"]
@include-section["version.scrbl"]

@index-section[]
