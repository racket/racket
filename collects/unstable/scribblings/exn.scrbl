#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/exn
                     racket/contract
                     racket/base))

@title[#:tag "exn"]{Exceptions}

@defmodule[unstable/exn]

@unstable-header[]

@defproc[(network-error [s symbol?]
                        [fmt string?]
                        [v any/c] ...)
         void]{
 Like @racket[error], but throws a @racket[exn:fail:network].
}

@defproc[(exn->string [exn (or/c exn? any/c)])
         string?]{
 Formats @racket[exn] with @racket[(error-display-handler)] as a string.
}
