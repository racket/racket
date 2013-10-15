#lang scribble/doc
@(require "common.rkt" (for-label net/tcp-redirect net/tcp-sig))

@title[#:tag "tcp-redirect"]{TCP Redirect: @racket[tcp^] via Channels}

@defmodule[net/tcp-redirect]{The @racketmodname[net/tcp-redirect]
library provides a function for directing some TCP port numbers to use
buffered channels instead of the TCP support from
@racketmodname[racket/tcp].}

@defproc[(tcp-redirect [port-numbers (listof (integer-in 0 65535))])
         unit?]{

Returns a unit that implements @racket[tcp^]. For port numbers not
listed in @racket[port-numbers], the unit's implementations are the
@racketmodname[racket/tcp] implementations.

For the port numbers listed in @racket[port-numbers] and for
connections to @racket["127.0.0.1"], the unit's implementation does
not use TCP connections, but instead uses internal buffered
channels. Such channels behave exactly as TCP listeners and ports.}
