#lang scribble/doc
@(require "common.ss"
          (for-label syntax/flatten-begin))

@title[#:tag "flatten-begin"]{Flattening @scheme[begin] Forms}

@defmodule[syntax/flatten-begin]

@defproc[(flatten-begin [stx syntax?]) (listof syntax?)]{

Extracts the sub-expressions from a @scheme[begin]-like form,
reporting an error if @scheme[stx] does not have the right shape
(i.e., a syntax list). The resulting syntax objects have annotations
transferred from @scheme[stx] using @scheme[syntax-track-origin].}
