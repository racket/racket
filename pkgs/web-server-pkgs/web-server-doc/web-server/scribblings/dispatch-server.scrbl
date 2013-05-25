#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "dispatch-server-unit"]{Dispatching Server}
@(require (for-label web-server/private/dispatch-server-unit
                     web-server/private/dispatch-server-sig
                     web-server/private/util
                     web-server/private/connection-manager
                     net/tcp-sig
                     unstable/contract
                     racket/async-channel
                     racket/tcp
                     web-server/web-server-sig))

The @web-server is just a configuration of a dispatching server.

@section{Dispatching Server Signatures}

@defmodule[web-server/private/dispatch-server-sig]{

The @racketmodname[web-server/private/dispatch-server-sig] library
provides two signatures.

@defsignature[dispatch-server^ ()]{

The @racket[dispatch-server^] signature is an alias for
@racket[web-server^].

 @defproc[(serve) (->* () (#:confirmation-channel (or/c false/c async-channel?)) (-> void))]{
  Runs the server---the confirmation channel will be send an exception if one occurs starting the server or the port number if there is none---and returns a procedure that shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          void]{
 Serves a single connection represented by the ports @racket[ip] and
 @racket[op].
 }
}

@defsignature[dispatch-server-config^ ()]{

 @defthing[port tcp-listen-port?]{Specifies the port to serve on.}
 @defthing[listen-ip (or/c string? false/c)]{Passed to @racket[tcp-listen].}
 @defthing[max-waiting exact-nonnegative-integer?]{Passed to @racket[tcp-listen].}
 @defthing[initial-connection-timeout integer?]{Specifies the initial timeout given to a connection.}
 @defproc[(read-request [c connection?]
                        [p tcp-listen-port?]
                        [port-addresses 
                         (input-port? . -> . (values string? string?))])
          (values any/c boolean?)]{
  Defines the way the server reads requests off connections to be passed
  to @racket[dispatch].
 }
 @defthing[dispatch (-> connection? any/c void)]{How to handle requests.}
}

}

@section{Dispatching Server Unit}

@defmodule[web-server/private/dispatch-server-unit]{

The @racketmodname[web-server/private/dispatch-server-unit] module
provides the unit that actually implements a dispatching server.

@defthing[dispatch-server@ (unit/c (import tcp^ dispatch-server-config^) 
                                   (export dispatch-server^))]{
 Runs the dispatching server config in a very basic way, except that it uses
 @secref["connection-manager"] to manage connections.
}

}

@section{Threads and Custodians}

The dispatching server runs in a dedicated thread. Every time a connection is initiated, a new thread is started to handle it.
Connection threads are created inside a dedicated custodian that is a child of the server's custodian. When the server is used to
provide servlets, each servlet also receives a new custodian that is a child of the server's custodian @bold{not} the connection
custodian.
