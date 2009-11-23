#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "configuration-table.ss"]{Configuration Table}
@(require (for-label web-server/configuration/configuration-table
                     web-server/configuration/configuration-table-structs
                     web-server/dispatchers/dispatch-log))

@defmodule[web-server/configuration/configuration-table]{

This module provides functions for
reading, writing, parsing, and printing @scheme[configuration-table]
structures.

@defthing[default-configuration-table-path path?]{The default configuration table S-expression file.}

@defthing[configuration-table-sexpr? (any . -> . boolean?)]{
 Equivalent to @scheme[list?].
}

@defproc[(sexpr->configuration-table (sexpr configuration-table-sexpr?))
         configuration-table?]{
 This function converts a @scheme[configuration-table] from an S-expression.
}

@defproc[(configuration-table->sexpr (ctable configuration-table?))
         configuration-table-sexpr?]{
 This function converts a @scheme[configuration-table] to an S-expression.
}

@schemeblock[
`((port ,integer?)
  (max-waiting ,integer?)
  (initial-connection-timeout ,integer?)
  (default-host-table
    ,host-table-sexpr?)
  (virtual-host-table
   (list ,symbol? ,host-table-sexpr?)
   ...))]

where a @scheme[host-table-sexpr] is:

@schemeblock[
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

In this syntax, the @scheme['messages] paths are relative to the
@scheme['configuration-root] directory.  All the paths in
@scheme['paths] except for @scheme['servlet-root] are relative to
@scheme['host-root] (other than @scheme['host-root] obviously.)
The @scheme['servlet-root] path is relative to @scheme['file-root].

Allowable @scheme['log-format]s are those accepted by @scheme[log-format->format].

Note: You almost always want to leave everything in the @scheme['paths] section the default except the @scheme['host-root].

@defproc[(read-configuration-table (path path-string?))
         configuration-table?]{
This function reads a @scheme[configuration-table] from @scheme[path].
}

@defproc[(write-configuration-table (ctable configuration-table?) (path path-string?))
         void]{
This function writes a @scheme[configuration-table] to @scheme[path].
}
              
}
