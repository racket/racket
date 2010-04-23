#lang scribble/doc
@(require "mz.ss")

@title[#:tag "concurrency" #:style 'toc]{Concurrency}

PLT Scheme supports multiple threads of control within a program,
thread-local storage, some primitive synchronization mechanisms, and a
framework for composing synchronization abstractions. In addition, the
@scheme[racket/future] library provides some support for parallelism
to improve performance.

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["threads.scrbl"]
@include-section["sync.scrbl"]
@include-section["thread-local.scrbl"]
@include-section["futures.scrbl"]
