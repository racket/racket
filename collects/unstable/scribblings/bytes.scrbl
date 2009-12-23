#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.ss"
          (for-label unstable/bytes
                     scheme/serialize
                     scheme/contract
                     scheme/base))

@title[#:tag "bytes"]{Bytes}

@defmodule[unstable/bytes]

@unstable-header[]

@defproc[(bytes-ci=? [b1 bytes?] [b2 bytes?]) boolean?]{
 Compares two bytes case insensitively.
}
                                                        
@defproc[(read/bytes [b bytes?])
         serializable?]{
 @scheme[read]s a value from @scheme[b] and returns it.
}

@defproc[(write/bytes [v serializable?])
         bytes?]{
 @scheme[write]s @scheme[v] to a bytes and returns it.
}
