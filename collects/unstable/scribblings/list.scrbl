#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label unstable/list
                     scheme/contract
                     scheme/base))

@title[#:tag "list"]{Lists}

@defmodule[unstable/list]

@defproc[(list-prefix? [l list?]
                       [r list?])
         boolean?]{
 True if @scheme[l] is a prefix of @scheme[r].
}