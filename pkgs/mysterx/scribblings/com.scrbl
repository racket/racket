#lang scribble/doc
@(require "common.rkt")

@title[#:tag "com" #:style 'toc]{COM}

MysterX allows scripting of most COM components from Racket.  A COM
component can be scripted in MysterX if it supports OLE Automation via
the @tt{IDispatch} interface, and if it publishes type information
using the @tt{ITypeInfo} interface.

@local-table-of-contents[]

@include-section["methprop.scrbl"]
@include-section["com-types.scrbl"]
@include-section["com-events.scrbl"]
