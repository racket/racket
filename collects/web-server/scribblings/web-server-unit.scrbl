#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "web-server-unit.ss"
             #:style 'toc]{Web Server Unit}
@(require (for-label web-server/web-server-sig
                     web-server/web-server-unit
                     net/tcp-sig
                     web-server/dispatchers/dispatch
                     web-server/web-config-sig
                     web-server/web-config-unit))

The @web-server offers a unit-based approach to running the server.

@local-table-of-contents[]

@section{Signature}

@defmodule[web-server/web-server-sig]{

@defsignature[web-server^ ()]{

 @defproc[(serve) (-> void)]{
  Runs the server and returns a procedure that shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          void]{
 Serves a single connection represented by the ports @scheme[ip] and
 @scheme[op].
 }
}

}

@section{Unit}

@defmodule[web-server/web-server-unit]{

@defthing[web-server@ (unit/c (web-config^ tcp^)
                              (web-server^))]{

Uses the @scheme[web-config^] to construct a @scheme[dispatcher/c]
function that sets up one virtual host dispatcher, for each virtual
host in the @scheme[web-config^], that sequences the following
operations:

@itemize[
 @item{Logs the incoming request with the given format to the given file}
 @item{Performs HTTP Basic Authentication with the given password file}
 @item{Allows the @scheme["/conf/refresh-passwords"] URL to refresh the password file.}
 @item{Allows the @scheme["/conf/collect-garbage"] URL to call the garbage collector.}
 @item{Allows the @scheme["/conf/refresh-servlets"] URL to refresh the servlets cache.}
 @item{Execute servlets in the mapping URLs to the given servlet root directory under htdocs.}
 @item{Serves files under the @scheme["/"] URL in the given htdocs directory.}
]

Using this @scheme[dispatcher/c], it loads a dispatching server that provides @scheme[serve]
and @scheme[serve-ports] functions that operate as expected.
}

}
