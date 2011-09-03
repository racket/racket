#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-server-unit"]{Server Units}
@(require (for-label web-server/web-server-sig
                     web-server/web-server-unit
                     net/tcp-sig
                     web-server/dispatchers/dispatch
                     web-server/web-config-sig
                     web-server/web-config-unit))

@section[#:tag "ws-sig" #:style 'hidden]{Signature}

@defmodule[web-server/web-server-sig]{

@defsignature[web-server^ ()]{

 @defproc[(serve) (-> void)]{
  Runs the server and returns a procedure that shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          void]{
 Serves a single connection represented by the ports @racket[ip] and
 @racket[op].
 }
}

}

@section[#:tag "ws-unit" #:style 'hidden]{Unit}

@defmodule[web-server/web-server-unit]{

@defthing[web-server@ (unit/c (web-config^ tcp^)
                              (web-server^))]{

Uses the @racket[web-config^] to construct a @racket[dispatcher/c]
function that sets up one virtual host dispatcher, for each virtual
host in the @racket[web-config^], that sequences the following
operations:

@itemize[
 @item{Logs the incoming request with the given format to the given file}
 @item{Performs HTTP Basic Authentication with the given password file}
 @item{Allows the @racket["/conf/refresh-passwords"] URL to refresh the password file.}
 @item{Allows the @racket["/conf/collect-garbage"] URL to call the garbage collector.}
 @item{Allows the @racket["/conf/refresh-servlets"] URL to refresh the servlets cache.}
 @item{Executes servlets mapping URLs to the given servlet root directory under htdocs.}
 @item{Serves files under the @racket["/"] URL in the given htdocs directory.}
]

Using this @racket[dispatcher/c], it loads a dispatching server that provides @racket[serve]
and @racket[serve-ports] functions that operate as expected.
}

}
