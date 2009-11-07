#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label unstable/exn
                     scheme/contract
                     scheme/base))

@title[#:tag "exn"]{Exceptions}

@defmodule[unstable/exn]

@defproc[(network-error [s symbol?]
                        [fmt string?]
                        [v any/c] ...)
         void]{
 Like @scheme[error], but throws a @scheme[exn:fail:network].
}

@defproc[(exn->string [exn (or/c exn? any/c)])
         string?]{
 Formats @scheme[exn] with @scheme[(error-display-handler)] as a string.
}
