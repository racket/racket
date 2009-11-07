#lang scribble/doc
@(require scribble/base
          scribble/manual)

@title[#:tag "unstable"]{Unstable}

@defmodule[unstable]

This manual documents some of the libraries available in the @schememodname[unstable] collection. 

The name @schememodname[unstable] is intended as a warning that the @bold{interfaces} in particular are unstable. Developers of planet packages and external projects should avoid using modules in the unstable collection. Contracts may change, names may change or disappear, even entire modules may move or disappear without warning to the outside world.

@local-table-of-contents[]

@include-section["bytes.scrbl"]
@include-section["contract.scrbl"]
@include-section["exn.scrbl"]
@include-section["list.scrbl"]
@include-section["net.scrbl"]
@include-section["path.scrbl"]
@include-section["string.scrbl"]