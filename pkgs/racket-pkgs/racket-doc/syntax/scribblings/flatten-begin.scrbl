#lang scribble/doc
@(require "common.rkt" (for-label syntax/flatten-begin))

@title[#:tag "flatten-begin"]{Flattening @racket[begin] Forms}

@defmodule[syntax/flatten-begin]

@defproc[(flatten-begin [stx syntax?]) (listof syntax?)]{

Extracts the sub-expressions from a @racket[begin]-like form,
reporting an error if @racket[stx] does not have the right shape
(i.e., a syntax list). The resulting syntax objects have annotations
transferred from @racket[stx] using @racket[syntax-track-origin].}
