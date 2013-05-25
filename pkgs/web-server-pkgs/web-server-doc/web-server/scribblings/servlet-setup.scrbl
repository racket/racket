#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "setup"]{Setting Up Servlets}
@(require (for-label web-server/servlet/setup
                     web-server/http
                     web-server/private/servlet
                     web-server/managers/manager
                     web-server/configuration/namespace
                     racket/serialize
                     web-server/stuffers/stuffer))

@defmodule[web-server/servlet/setup]{

This module is used internally to build and load servlets. It may be useful to those who are trying to extend the server.

@defproc[(make-v1.servlet [directory path-string?]
                          [timeout integer?]
                          [start (request? . -> . can-be-response?)])
         servlet?]{
 Creates a version 1 servlet that uses @racket[directory] as its current directory, a timeout manager with a @racket[timeout] timeout, and @racket[start] as the request handler.
}

@defproc[(make-v2.servlet [directory path-string?]
                          [manager manager?]
                          [start (request? . -> . can-be-response?)])
         servlet?]{
 Creates a version 2 servlet that uses @racket[directory] as its current directory, a @racket[manager] as the continuation manager, and @racket[start] as the request handler.
}
 
@defproc[(make-stateless.servlet [directory path-string?]
                                 [stuffer (stuffer/c serializable? bytes?)]
                                 [manager manager?]
                                 [start (request? . -> . can-be-response?)])
         servlet?]{
 Creates a stateless @racketmodname[web-server] servlet that uses @racket[directory] as its current directory, @racket[stuffer] as its stuffer, and @racket[manager] as the continuation manager, and @racket[start] as the request handler.
}
                  
@defthing[default-module-specs (listof module-path?)]{
 The modules that the Web Server needs to share with all servlets.
}

@defthing[path->servlet/c contract?]{
Equivalent to @racket[(path? . -> . servlet?)].
}

@defproc[(make-default-path->servlet 
          [#:make-servlet-namespace
           make-servlet-namespace
           make-servlet-namespace/c
           (make-make-servlet-namespace)]
          [#:timeouts-default-servlet
           timeouts-default-servlet
           integer?
           30])
         path->servlet/c]{
 Constructs a procedure that loads a servlet from the path in a namespace created with @racket[make-servlet-namespace],
 using a timeout manager with @racket[timeouts-default-servlet] as the default timeout (if no manager is given.)
}

}
