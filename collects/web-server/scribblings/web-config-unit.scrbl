#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-config-unit" #:tag-prefix "web-config"]{Configuration Units}
@(require (for-label web-server/web-config-unit
                     web-server/configuration/namespace
                     web-server/configuration/configuration-table
                     web-server/configuration/configuration-table-structs
                     web-server/private/util
                     web-server/servlet/setup
                     racket/tcp
                     (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
                     web-server/web-config-sig))

@section[#:style 'hidden]{Signature}

@defmodule[web-server/web-config-sig]{

@defsignature[web-config^ ()]{

@signature-desc{
Provides contains the following identifiers.
}

@defthing[max-waiting exact-nonnegative-integer?]{
 Passed to @racket[tcp-accept].
}

@defthing[virtual-hosts (string? . -> . host?)]{
 Contains the configuration of individual virtual hosts.
}

@defthing[initial-connection-timeout integer?]{
 Specifies the initial timeout given to a connection.
}

@defthing[port port-number?]{
 Specifies the port to serve HTTP on.
}

@defthing[listen-ip (or/c false/c string?)]{
 Passed to @racket[tcp-listen].
}

@defthing[make-servlet-namespace make-servlet-namespace/c]{
 Passed to @racket[servlets:make] through @racket[make-default-path->servlet].
}
}
             
}

@section[#:style 'hidden]{Unit}

@defmodule[web-server/web-config-unit]{

@defproc[(configuration-table->web-config@ [path path-string?]
                                           [#:port port (or/c false/c port-number?) #f]
                                           [#:listen-ip listen-ip (or/c false/c string?) #f]
                                           [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace/c (make-make-servlet-namespace)])
         (unit/c (import) (export web-config^))]{
 Reads the S-expression at @racket[path] and calls
 @racket[configuration-table-sexpr->web-config@] appropriately.
}

@defproc[(configuration-table-sexpr->web-config@ [sexpr list?]
                                                 [#:web-server-root web-server-root path-string?
                                                                    (directory-part default-configuration-table-path)]
                                                 [#:port port (or/c false/c port-number?) #f]
                                                 [#:listen-ip listen-ip (or/c false/c string?) #f]
                                                 [#:make-servlet-namespace make-servlet-namespace make-servlet-namespace/c
                                                                           (make-make-servlet-namespace)])
         (unit/c (import) (export web-config^))]{
 Parses @racket[sexpr] as a configuration-table and constructs a @racket[web-config^] unit.
}

}
