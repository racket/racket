#lang scribble/doc
@(require "common.rkt")

@title{MysterX: Legacy Support for Windows COM}

@author["Paul Steckler"]

@bold{MysterX} allows scripting of most COM components from Racket.  A COM
component can be scripted in MysterX if it supports OLE Automation via
the @tt{IDispatch} interface, and if it publishes type information
using the @tt{ITypeInfo} interface.

@yellow{@bold{NOTE:}} MysterX is supported but depracated; use
@racketmodname[ffi/com] or @racketmodname[ffi/unsafe/com], instead. 
MysterX formerly provided @as-index{ActiveX}
support, but ActiveX support has been discontinued.

@defmodule[mysterx]

@table-of-contents[]

@include-section["methprop.scrbl"]
@include-section["com-types.scrbl"]
@include-section["com-events.scrbl"]
@include-section["version.scrbl"]

@index-section[]
