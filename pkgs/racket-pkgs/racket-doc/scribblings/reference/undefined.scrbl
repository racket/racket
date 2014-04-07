#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/undefined))

@title[#:tag "undefined"]{Undefined}

@note-lib[racket/undefined]

The constant @racket[undefined] can be used as a placeholder value for
a value to be installed later, especially for cases where premature
access of the value is either difficult or impossible to detect or
prevent.

The @racket[undefined] value is always @racket[eq?] to itself.

@history[#:added "6.0.0.6"]

@defproc[(undefined? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is the constant @racket[undefined],
@racket[#f] otherwise.}

@defthing[undefined undefined?]{The ``undefined'' constant.}
