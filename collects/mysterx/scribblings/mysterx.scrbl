#lang scribble/doc
@(require "common.ss")

@title{@bold{MysterX}: Using Windows COM Objects in Scheme}

@bold{MysterX} is a toolkit for building Windows applications from
ActiveX and COM components, using Scheme as glue code.  Dynamic HTML
is used for component presentation and event-handling.  MysterX
requires Internet Explorer (IE) 4 or later to be installed.
Distributed COM (DCOM) for your version of Windows is also required.
Recent versions of Windows come with DCOM; DCOM packages for Windows
95 and 98 are made available separately.

@defmodule[mysterx]

@table-of-contents[]

@include-section["overview.scrbl"]
@include-section["version.scrbl"]
@include-section["browsers.scrbl"]
@include-section["documents.scrbl"]
@include-section["html-events.scrbl"]
@include-section["com-types.scrbl"]
@include-section["com.scrbl"]
@include-section["dcom.scrbl"]
@include-section["com-events.scrbl"]
@include-section["types.scrbl"]
@include-section["html.scrbl"]
