#lang scribble/doc
@(require scribble/base scribble/manual "utils.rkt"
          (for-label racket/base unstable/bytes
                     racket/contract))

@title[#:tag "bytes"]{Bytes}
@unstable-header[]

@defmodule[unstable/bytes]

@defproc[(read/bytes [b bytes?])
         printable/c]{
 @racket[read]s a value from @racket[b] and returns it.
}

@defproc[(write/bytes [v printable/c])
         bytes?]{
 @racket[write]s @racket[v] to a bytes and returns it.
}
