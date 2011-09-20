#lang scribble/doc
@(require scribble/manual "common.rkt")

@title[#:tag "ctool" #:style 'toc]{@exec{raco ctool}: Working with C Code}

The @exec{raco ctool} command works in various modes (as determined by
command-line flags) to support various tasks involving C code.

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@include-section["cc.scrbl"]
@include-section["c-mods.scrbl"]
@include-section["api.scrbl"]

