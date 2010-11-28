#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-server"]{Launching Servers}

@(require (for-label web-server/web-server
                     web-server/dispatchers/filesystem-map
                     web-server/web-config-unit
                     web-server/web-config-sig
                     web-server/private/dispatch-server-unit
                     web-server/private/dispatch-server-sig
                     web-server/dispatchers/dispatch
                     net/tcp-sig
                     racket/async-channel
                     unstable/contract
                     web-server/configuration/configuration-table)
          (prefix-in raw: (for-label net/tcp-unit))
          (prefix-in files: (for-label web-server/dispatchers/dispatch-files)))

@defmodule[web-server/web-server]{

This module provides functions for launching dispatching servers.

@defproc[(serve [#:dispatch dispatch dispatcher/c]
                [#:confirmation-channel confirmation-channel (or/c false/c async-channel?) #f]
                [#:connection-close? connection-close? boolean? #f]
                [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                [#:port port tcp-listen-port? 80]
                [#:listen-ip listen-ip (or/c string? false/c) #f]
                [#:max-waiting max-waiting integer? 40]
                [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Constructs an appropriate @racket[dispatch-server-config^], invokes the
 @racket[dispatch-server@], and calls its @racket[serve] function.
 
 If @racket[connection-close?] is @racket[#t], then every connection is closed after one
 request. Otherwise, the client decides based on what HTTP version it uses.
 
 The @racket[#:tcp@] keyword is provided for building an SSL server. See @secref["faq:https"].
}

Here's an example of a simple web server that serves files
from a given path:

@racketblock[
(define (start-file-server base)
  (serve
   #:dispatch
   (files:make
    #:url->path (make-url->path base)
    #:path->mime-type
    (lambda (path)
      #"application/octet-stream"))
   #:port 8080))
]

@defproc[(serve/ports [#:dispatch dispatch dispatcher/c]
                      [#:confirmation-channel confirmation-channel (or/c false/c async-channel?) #f]
                      [#:connection-close? connection-close? boolean? #f]
                      [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                      [#:ports ports (listof tcp-listen-port?) (list 80)]
                      [#:listen-ip listen-ip (or/c string? false/c) #f]
                      [#:max-waiting max-waiting integer? 40]
                      [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @racket[serve] multiple times, once for each @racket[port], and returns
 a function that shuts down all of the server instances.
}

@defproc[(serve/ips+ports [#:dispatch dispatch dispatcher/c]
                          [#:confirmation-channel confirmation-channel (or/c false/c async-channel?) #f]
                          [#:connection-close? connection-close? boolean? #f]
                          [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                          [#:ips+ports ips+ports (listof (cons/c (or/c string? false/c) (listof tcp-listen-port?))) (list (cons #f (list 80)))]
                          [#:max-waiting max-waiting integer? 40]
                          [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @racket[serve/ports] multiple times, once for each @racket[ip], and returns
 a function that shuts down all of the server instances.
}
                  
@defproc[(serve/web-config@ [config@ (unit/c (import) (export web-config^))]
                            [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@])
         (-> void)]{
 Starts the @web-server with the settings defined by the given @racket[web-config^] unit.
        
 It is very useful to combine this with @racket[configuration-table->web-config@] and @racket[configuration-table-sexpr->web-config@]:
 
 @racketblock[
  (serve/web-config@
   (configuration-table->web-config@ 
    default-configuration-table-path))]
}

@defproc[(do-not-return) void]{
 This function does not return. If you are writing a script to load the @web-server
 you are likely to want to call this functions at the end of your script.
}

}

@include-section["servlet-env-int.scrbl"]
