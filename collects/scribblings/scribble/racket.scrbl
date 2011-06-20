#lang scribble/manual
@(require "utils.rkt" (for-label help/search))

@title[#:style 'toc]{Connecting to @filepath{racket}}

This library is dynamically loaded by @tt{racket}'s
@racket[help] function.

@defmodule[help/help-utils]

@defproc[(search-for [strs (listof string?)]) void?]{
  Calls @racket[perform-search] after concatenating the
  elements of @racket[strs] and adding spaces between them.
}
@defproc[(find-help/lib [id symbol?] [lib module-path?]) void?]{
  Visits the documentation page for @racket[id] as an export of @racket[lib].
}

@defproc[(find-help [id identifier?]) void?]{
  Visits the documentation for @racket[id].
}

@defproc[(go-to-main-page) void?]{
  Visits the main entry page for the documentation.
}
