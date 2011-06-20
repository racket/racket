#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "all-sync" #:style 'toc]{Synchronization}

Racket's synchronization toolbox spans three layers:

@itemize[

@item{@tech{synchronizable events} --- a general framework for
synchronization;}

@item{@tech{channels} --- a primitive that can be used, in principle,
to build most other kinds of synchronizable events (except the ones
that compose events); and}

@item{@tech{semaphores} --- a simple and especially cheap primitive
for synchronization.}

]


@local-table-of-contents[]

@include-section["evts.scrbl"]
@include-section["channels.scrbl"]
@include-section["semaphores.scrbl"]
@include-section["async-channels.scrbl"]
