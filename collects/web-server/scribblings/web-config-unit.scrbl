#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "web-config-unit.ss"]{Web Config Unit}
@(require (for-label web-server/web-config-unit)
          (for-label web-server/web-config-sig))

The @web-server offers a unit-based approach to configuring the server.

@section{Configuration Signature}

@defmodule[web-server/web-config-sig]

@defsignature[web-config^ ()]{

@signature-desc{
Provides contains the following identifiers.
}

@defthing[max-waiting integer?]{
 Passed to @scheme[tcp-accept].
}

@defthing[virtual-hosts (listof (cons/c string? host-table?))]{
 Contains the configuration of individual virtual hosts.
}

@defthing[scripts (box/c (cache-table? path? servlet?))]{
 Contains initially loaded servlets.
}

@defthing[initial-connection-timeout integer?]{
 Specifies the initial timeout given to a connection.
}

@defthing[port port-number?]{
 Specifies the port to serve HTTP on.
}

@defthing[listen-ip string?]{
 Passed to @scheme[tcp-accept].
}

@defthing[make-servlet-namespace make-servlet-namespace?]{
 Passed to @scheme[servlets:make].
}
}

@section{Configuration Units}

@defmodule[web-server/web-config-unit]

@defproc[(configuration-table->web-config\@ [path path?]
                                           [#:port port (or/c false/c port-number?) #f]
                                           [#:listen-ip listen-ip (or/c false/c string?) #f]
                                           [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace? (make-make-servlet-namespace)])
         (unit? web-config^)]{
 Reads the S-expression at @scheme[path] and calls
 @scheme[configuration-table-sexpr->web-config@] appropriately.
}

@defproc[(configuration-table-sexpr->web-config\@ [sexpr list?]
                                                 [#:web-server-root web-server-root path? (directory-part default-configuration-table-path)]
                                                 [#:port port (or/c false/c port-number?) #f]
                                                 [#:listen-ip listen-ip (or/c false/c string?) #f]
                                                 [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace? (make-make-servlet-namespace)])
         (unit? web-config^)]{
 Parses @scheme[sexpr] as a configuration-table and constructs a @scheme[web-config^] unit.
}
