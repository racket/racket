#lang scribble/doc
@(require "common.rkt")

@title{MysterX: Using Windows COM Objects in Racket}

@author["Paul Steckler"]

@bold{MysterX} is a toolkit for building Windows applications from
@as-index{ActiveX} and COM components, using Racket as glue code.
Dynamic HTML (DHTML) is used for component presentation and
event-handling.

@yellow{@bold{WARNING:}} All ActiveX support (as described in
@secref["activex"]) is scheduled for removal from MysterX after
version 5.2.1.

@defmodule[mysterx]

@table-of-contents[]

@include-section["overview.scrbl"]
@include-section["com.scrbl"]
@include-section["activex.scrbl"]
@include-section["version.scrbl"]

@index-section[]
