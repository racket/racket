#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "configuration"
       #:style 'toc]{Configuration}

There are a number of libraries and utilities useful for
configuring the @web-server .

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "configuration-table-structs.ss"]{Configuration Table Structure}
@(require (for-label web-server/configuration/configuration-table-structs
                     web-server/http
                     net/url
                     web-server/private/util))

@defmodule[web-server/configuration/configuration-table-structs]{

@filepath{configuration/configuration-table-structs.ss} provides the following structures that
represent a standard configuration (see @secref["web-server-unit.ss"]) of the @web-server .
The contracts on this structure influence the valid types of values in
the configuration table S-expression file format described in
@secref["configuration-table.ss"].

@defstruct[configuration-table
           ([port port-number?]
            [max-waiting natural-number/c]
            [initial-connection-timeout natural-number/c]
            [default-host host-table?]
            [virtual-hosts (listof (cons/c string? host-table?))])]

@defstruct[host-table
           ([indices (listof string?)]
            [log-format symbol?]
            [messages messages?]
            [timeouts timeouts?]
            [paths paths?])]

@defstruct[host
           ([indices (listof string?)]
            [log-format symbol?]
            [log-path (or/c false/c path-string?)]
            [passwords (or/c false/c path-string?)]
            [responders responders?]
            [timeouts timeouts?]
            [paths paths?])]

@defstruct[responders
           ([servlet (url? any/c . -> . response?)]
            [servlet-loading (url? any/c . -> . response?)]
            [authentication (url? (cons/c symbol? string?) . -> . response?)]
            [servlets-refreshed (-> response?)]
            [passwords-refreshed (-> response?)]
            [file-not-found (request? . -> . response?)]
            [protocol (url? . -> . response?)]
            [collect-garbage (-> response?)])]

@defstruct[messages
           ([servlet string?]
            [authentication string?]
            [servlets-refreshed string?]
            [passwords-refreshed string?]
            [file-not-found string?]
            [protocol string?]
            [collect-garbage string?])]

@defstruct[timeouts
           ([default-servlet number?]
            [password number?]
            [servlet-connection number?]
            [file-per-byte number?]
            [file-base number?])]

@defstruct[paths
           ([conf (or/c false/c path-string?)]
            [host-base (or/c false/c path-string?)]
            [log (or/c false/c path-string?)]
            [htdocs (or/c false/c path-string?)]
            [servlet (or/c false/c path-string?)]
            [mime-types (or/c false/c path-string?)]
            [passwords (or/c false/c path-string?)])]

}

@; ------------------------------------------------------------
@section[#:tag "configuration-table.ss"]{Configuration Table}
@(require (for-label web-server/configuration/configuration-table))
@(require (for-label web-server/dispatchers/dispatch-log))

@defmodule[web-server/configuration/configuration-table]{

@filepath{configuration/configuration-table.ss} provides functions for
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

In this syntax, the @scheme['messages] paths are relative to the @scheme['configuration-root] directory.
All the paths in @scheme['paths] are relative to @scheme['host-root] (other than @scheme['host-root] obviously.)

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

@; ------------------------------------------------------------
@section[#:tag "namespace.ss"]{Servlet Namespaces}
@(require (for-label web-server/configuration/namespace))

@defmodule[web-server/configuration/namespace]{

@filepath{configuration/namespace.ss} provides a function to help create the
@scheme[make-servlet-namespace] procedure needed by the @scheme[make] functions
of @filepath{dispatchers/dispatch-servlets.ss} and @filepath{dispatchers/dispatch-lang.ss}.

@defthing[make-servlet-namespace/c contract?]{
 Equivalent to 
 @schemeblock[
(->* ()
     (#:additional-specs (listof module-path?))
     namespace?)
].
}

@defproc[(make-make-servlet-namespace (#:to-be-copied-module-specs to-be-copied-module-specs (listof module-path?)))
         make-servlet-namespace/c]{
This function creates a function that when called will construct a new @scheme[namespace] that
has all the modules from @scheme[to-be-copied-module-specs] and @scheme[additional-specs], as well
as @scheme[mzscheme] and @scheme[mred], provided they are already attached
to the @scheme[(current-namespace)] of the call-site.

Example:
@schemeblock[
 (make-make-servlet-namespace
  #:to-be-copied-module-specs `((lib "database.ss" "my-module")))
 ]
}
                                  
}

@subsection{Why this is useful}

A different namespace is needed for each servlet, so that if servlet A and servlet B both use
a stateful module C, they will be isolated from one another. We see the @web-server as
an operating system for servlets, so we inherit the isolation requirement on operating systems.

However, there are some modules which must be shared. If they were not, then structures cannot
be passed from the @web-server to the servlets, due to a subtlety in the way MzScheme
implements structures.

Since, on occasion, a user will actually wanted servlets A and B to interact through module C.
A custom @scheme[make-servlet-namespace] can be created, through this procedure, that attaches
module C to all servlet namespaces. Through other means (see @secref["dispatchers"]) different sets
of servlets can share different sets of modules.

@; ------------------------------------------------------------
@section[#:tag "responders.ss"]{Standard Responders}
@(require (for-label web-server/configuration/responders))

@defmodule[web-server/configuration/responders]{

@filepath{configuration/responders.ss} provides some functions that help constructing HTTP responders.
These functions are used by the default dispatcher constructor (see @secref["web-server-unit.ss"]) to
turn the paths given in the @scheme[configuration-table] into responders for the associated circumstance.

@defproc[(file-response (http-code natural-number/c) (short-version string?) (text-file string?) (header header?) ...)
         response?]{
 Generates a @scheme[response/full] with the given @scheme[http-code] and @scheme[short-version]
as the corresponding fields; with the content of the @scheme[text-file] as the body; and, with
the @scheme[header]s as, you guessed it, headers.
}

@defproc[(servlet-loading-responder (url url?) (exn exn?))
         response?]{
 Gives @scheme[exn] to the @scheme[current-error-handler] and response with a stack trace and a "Servlet didn't load" message.
}

@defproc[(gen-servlet-not-found (file path-string?))
         ((url url?) . -> . response?)]{
 Returns a function that generates a standard "Servlet not found." error with content from @scheme[file].
}

@defproc[(servlet-error-responder (url url?) (exn exn?))
         response?]{
 Gives @scheme[exn] to the @scheme[current-error-handler] and response with a stack trace and a "Servlet error" message.
}
                                       
@defproc[(gen-servlet-responder (file path-string?))
         ((url url?) (exn any/c) . -> . response?)]{
 Prints the @scheme[exn] to standard output and responds with a "Servlet error." message with content from @scheme[file].
}

@defproc[(gen-servlets-refreshed (file path-string?))
         (-> response?)]{
 Returns a function that generates a standard "Servlet cache refreshed." message with content from @scheme[file].
}

@defproc[(gen-passwords-refreshed (file path-string?))
         (-> response?)]{
 Returns a function that generates a standard "Passwords refreshed." message with content from @scheme[file].
}

@defproc[(gen-authentication-responder (file path-string?))
         ((url url?) (header header?) . -> . response?)]{
 Returns a function that generates an authentication failure error with content from @scheme[file] and
@scheme[header] as the HTTP header.
}

@defproc[(gen-protocol-responder (file path-string?))
         ((url url?) . -> . response?)]{
 Returns a function that generates a "Malformed request" error with content from @scheme[file].
}

@defproc[(gen-file-not-found-responder (file path-string?))
         ((req request?) . -> . response?)]{
 Returns a function that generates a standard "File not found" error with content from @scheme[file].
}

@defproc[(gen-collect-garbage-responder (file path-string?))
         (-> response?)]{
 Returns a function that generates a standard "Garbage collection run" message with content from @scheme[file].
}

}
