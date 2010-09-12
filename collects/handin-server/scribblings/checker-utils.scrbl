#lang scribble/doc
@(require "common.rkt")

@title[#:style 'toc]{Checker Utilities}

The checker utilities are provided to make writing checker functions.
They are provided in a few layers, each layer provides new
functionality in addition to the lower one.  These modules are (in
order):

@itemize[

@item{@racketmodname[racket/sandbox]: is the basic sandbox
  evaluation code, which is the basic functionality checkers build on.}

@item{@racketmodname[handin-server/sandbox]: contains a wrapper that
  configures the Racket sandbox for the handin server.}

@item{@racketmodname[handin-server/utils]: contains additional
  utilities for dealing with handin submissions, as well as a few
  helpers for testing code.}

@item{@racketmodname[handin-server/checker]: automates the task of
  creating a checker function (in
  @filepath{<active-assignment>/checker.rkt} modules) to cope with
  common submission situations.}]

The following sections describe each of these modules.

@local-table-of-contents[]

@include-section["sandbox.scrbl"]
@include-section["utils.scrbl"]
@include-section["checker.scrbl"]
