#lang scribble/doc
@(require "mz.ss")

@title[#:tag "concurrency" #:style 'toc]{Concurrency}

PLT Scheme supports multiple threads of control within a program,
thread-local storage, some primitive synchronization mechanisms, and a
framework for composing synchronization abstractions.

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["threads.scrbl"]
@include-section["sync.scrbl"]
@include-section["thread-local.scrbl"]


