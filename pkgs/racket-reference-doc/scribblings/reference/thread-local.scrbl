#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "thread-local-storage" #:style 'toc]{Thread-Local Storage}

@tech{Thread cells} provides primitive support for thread-local
storage. @tech{Parameters} combine thread cells and continuation marks
to support thread-specific, continuation-specific binding.

@local-table-of-contents[]

@include-section["thread-cells.scrbl"]
@include-section["parameters.scrbl"]
