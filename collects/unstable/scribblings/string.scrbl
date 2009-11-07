#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label unstable/string
                     scheme/serialize
                     scheme/contract
                     scheme/base))

@title[#:tag "string"]{Strings}

@defmodule[unstable/string]

@defproc[(lowercase-symbol! [sb (or/c string? bytes?)])
         symbol?]{
 Returns @scheme[sb] as a lowercase symbol.
}

@defproc[(read/string [s string?])
         serializable?]{
 @scheme[read]s a value from @scheme[s] and returns it.
}

@defproc[(write/string [v serializable?])
         string?]{
 @scheme[write]s @scheme[v] to a string and returns it.
}
