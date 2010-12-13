#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "configuration-table"]{Configuration Table}
@(require (for-label web-server/configuration/configuration-table
                     web-server/configuration/configuration-table-structs
                     web-server/dispatchers/dispatch-log))

@defmodule[web-server/configuration/configuration-table]{

This module provides functions for
reading, writing, parsing, and printing @racket[configuration-table]
structures.

@defthing[default-configuration-table-path path?]{The default configuration table S-expression file.}

@defthing[configuration-table-sexpr? (any . -> . boolean?)]{
 Equivalent to @racket[list?].
}

@defproc[(sexpr->configuration-table (sexpr configuration-table-sexpr?))
         configuration-table?]{
 This function converts a @racket[configuration-table] from an S-expression.
}

@defproc[(configuration-table->sexpr (ctable configuration-table?))
         configuration-table-sexpr?]{
 This function converts a @racket[configuration-table] to an S-expression.
}

@racketblock[
`((port ,integer?)
  (max-waiting ,exact-integer?)
  (initial-connection-timeout ,integer?)
  (default-host-table
    ,host-table-sexpr?)
  (virtual-host-table
   (list ,symbol? ,host-table-sexpr?)
   ...))]

where a @racket[host-table-sexpr] is:

@racketblock[
`(host-table
  (default-indices ,string? ...)
  (log-format ,symbol?)
  (messages
   (servlet-message ,path-string?)
   (authentication-message ,path-string?)
   (servlets-refreshed ,path-string?)
   (passwords-refreshed ,path-string?)
   (file-not-found-message ,path-string?)
   (protocol-message ,path-string?)
   (collect-garbage ,path-string?))
  (timeouts
   (default-servlet-timeout ,integer?)
   (password-connection-timeout ,integer?)
   (servlet-connection-timeout ,integer?)
   (file-per-byte-connection-timeout ,integer?)
   (file-base-connection-timeout ,integer))
  (paths
   (configuration-root ,path-string?)
   (host-root ,path-string?)
   (log-file-path ,path-string?)
   (file-root ,path-string?)
   (servlet-root ,path-string?)
   (mime-types ,path-string?)
   (password-authentication ,path-string?)))]

In this syntax, the @racket['messages] paths are relative to the
@racket['configuration-root] directory.  All the paths in
@racket['paths] except for @racket['servlet-root] are relative to
@racket['host-root] (other than @racket['host-root] obviously.)
The @racket['servlet-root] path is relative to @racket['file-root].

Allowable @racket['log-format]s are those accepted by @racket[log-format->format].

Note: You almost always want to leave everything in the @racket['paths] section the default except the @racket['host-root].

@defproc[(read-configuration-table (path path-string?))
         configuration-table?]{
This function reads a @racket[configuration-table] from @racket[path].
}

@defproc[(write-configuration-table (ctable configuration-table?) (path path-string?))
         void]{
This function writes a @racket[configuration-table] to @racket[path].
}
              
}
