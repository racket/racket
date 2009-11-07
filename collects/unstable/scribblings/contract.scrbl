#lang scribble/doc
@(require scribble/base
          scribble/manual
          (for-label unstable/contract
                     scheme/contract
                     scheme/base))

@title[#:tag "contract"]{Contracts}

@defmodule[unstable/contract]

@defthing[non-empty-string/c contract?]{Contract for non-empty strings.}

@defthing[port-number? contract?]{Equivalent to @scheme[(between/c 1 65535)].}

@defthing[path-element? contract?]{Equivalent to @scheme[(or/c path-string? (symbols 'up 'same))].}
