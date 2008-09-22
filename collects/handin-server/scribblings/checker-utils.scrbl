#lang scribble/doc
@(require "common.ss")

@title[#:style 'toc]{Checker Utilities}

The checker utilities are provided to make writing checker functions.
They are provided in a few layers, each layer provides new
functionality in addition to the lower one.  These modules are (in
order):

@itemize[

@item{@schememodname[scheme/sandbox]: contains basic sandbox
  evaluation utilities.  This is in MzLib since it can be used
  independently.}

@item{@schememodname[handin-server/sandbox]: contains a wrapper that
  configures MzLib's sandbox for the handin server.}

@item{@schememodname[handin-server/utils]: contains additional
  utilities for dealing with handin submissions, as well as a few
  helpers for testing code.}

@item{@schememodname[handin-server/checker]: automates the task of
  creating a checker function (in
  @filepath{<active-assignment>/checker.ss} modules) to cope with
  common submission situations.}]

The following sections describe each of these modules.

@local-table-of-contents[]

@include-section["sandbox.scrbl"]
@include-section["utils.scrbl"]
@include-section["checker.scrbl"]
