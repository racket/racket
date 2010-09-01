#lang scribble/doc
@(require "base.rkt")

@title[#:tag "api"]{RackUnit API}

@defmodule[rackunit
  #:use-sources (rackunit)]

@include-section["overview.scrbl"]
@include-section["check.scrbl"]
@include-section["compound-testing.scrbl"]
@include-section["control-flow.scrbl"]
@include-section["misc.scrbl"]
@include-section["ui.scrbl"]
