#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label unstable/bytes
                     racket/serialize
                     racket/contract
                     racket/base))

@title[#:tag "bytes"]{Bytes}

@defmodule[unstable/bytes]

@unstable-header[]

@defproc[(bytes-ci=? [b1 bytes?] [b2 bytes?]) boolean?]{
 Compares two bytes case insensitively.
}
                                                        
@defproc[(read/bytes [b bytes?])
         serializable?]{
 @racket[read]s a value from @racket[b] and returns it.
}

@defproc[(write/bytes [v serializable?])
         bytes?]{
 @racket[write]s @racket[v] to a bytes and returns it.
}
