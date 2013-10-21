#lang scribble/manual
@(require "utils.rkt"
          (for-label racket/base
                     racket/contract
                     unstable/socket))

@title[#:tag "unix-socket"]{Unix Domain Sockets}
@unstable-header[]

@defmodule[unstable/socket]

@defthing[unix-socket-available?
          boolean?]{

A boolean value that indicates whether unix domain sockets are
available and supported on the current platform. The supported
platforms are Linux and Mac OS X; unix domain sockets are not
supported on Windows and other Unix variants.
}

@defproc[(unix-socket-connect [socket-path unix-socket-path?])
         (values input-port? output-port?)]{

Connects to the unix domain socket associated with
@racket[socket-path] and returns an input port and output port for
communicating with the socket.
}

@defproc[(unix-socket-path? [v any/c]) boolean?]{

Predicate that identifies a valid unix domain socket path for this
system.  This is equivalent to @racket[path-string?] on Mac OS X,
but also includes an arbitrary string with @racket[\u0000] prefix
on Linux (to accomodate for it's abstract namespace feature).
}
