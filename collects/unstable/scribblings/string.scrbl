#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/string
                     racket/serialize
                     racket/contract
                     racket/base))

@title[#:tag "string"]{Strings}

@defmodule[unstable/string]

@unstable-header[]

@defproc[(lowercase-symbol! [sb (or/c string? bytes?)])
         symbol?]{
 Returns @racket[sb] as a lowercase symbol.
}

@defproc[(read/string [s string?])
         serializable?]{
 @racket[read]s a value from @racket[s] and returns it.
}

@defproc[(write/string [v serializable?])
         string?]{
 @racket[write]s @racket[v] to a string and returns it.
}
