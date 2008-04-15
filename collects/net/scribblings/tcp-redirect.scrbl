#lang scribble/doc
@(require "common.ss"
          (for-label net/tcp-redirect
                     net/tcp-sig))

@title[#:tag "tcp-redirect"]{TCP Redirect: @scheme[tcp^] via Channels}

@defmodule[net/tcp-redirect]{The @schememodname[net/tcp-redirect]
library provides a function for directing some TCP port numbers to use
buffered channels instead of the TCP support from
@schememodname[scheme/tcp].}

@defproc[(tcp-redirect [port-numbers (listof (integer-in 0 65535))])
         unit?]{

Returns a unit that implements @scheme[tcp^]. For port numbers not
listed in @scheme[port-numbers], the unit's implementations are the
@schememodname[scheme/tcp] implementations.

For the port numbers listed in @scheme[port-numbers] and for
connections to @scheme["127.0.0.1"], the unit's implementation does
not use TCP connections, but instead uses internal buffered
channels. Such channels behave exactly as TCP listeners and ports.}
