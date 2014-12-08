#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "concurrency" #:style 'toc]{Concurrency and Parallelism}

Racket supports multiple threads of control within a program,
thread-local storage, some primitive synchronization mechanisms, and a
framework for composing synchronization abstractions. In addition, the
@racket[racket/future] and @racket[racket/place] libraries provide
support for parallelism to improve performance.

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["threads.scrbl"]
@include-section["sync.scrbl"]
@include-section["thread-local.scrbl"]
@include-section["futures.scrbl"]
@include-section["places.scrbl"]
@include-section["engine.scrbl"]
