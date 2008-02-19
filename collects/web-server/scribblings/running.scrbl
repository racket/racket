#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "run.ss"
       #:style 'toc]{Running the Web Server}

There are a number of ways to run the Web Server. The two primary ways
are through a command-line tool or through a function call.

@local-table-of-contents[]

@; ------------------------------------------------------------
@section[#:tag "command-line-tools"]{Command-line Tools}

One command-line utility is provided with the @|web-server|:

@commandline{plt-web-server [-f <file-name> -p <port> -a <ip-address>]}

The optional file-name argument specifies the path to a
@scheme[configuration-table] S-expression (see
@secref["configuration-table.ss"].) If this is not provided, the
default configuration shipped with the server is used. The optional
port and ip-address arguments override the corresponding portions of
the @scheme[configuration-table].

The @scheme[configuration-table] is given to @scheme[configuration-table->web-config\@]
and used to construct a @scheme[web-config^] unit,
and is linked with the @scheme[web-server\@] unit. The resulting unit is invoked, and
the server runs until the process is killed.

To run the web server with MrEd, use

@commandline{mred -l- web-server/gui [-f <file-name> -p <port> -a <ip-address>]}

@; ------------------------------------------------------------
@section[#:tag "web-server.ss"]{Functional}
@(require (for-label web-server/web-server))

@defmodule[web-server/web-server]

@filepath{web-server.ss} provides a number of functions for easing embedding
of the @web-server in other applications, or loading a custom
dispatcher. See @filepath{run.ss} for an example of such a script.

@defproc[(serve [#:dispatch dispatch dispatcher?]
                [#:tcp\@ tcp\@ tcp-unit^ raw:tcp\@]
                [#:port port integer? 80]
                [#:listen-ip listen-ip (or/c string? false/c) #f]
                [#:max-waiting max-waiting integer? 40]
                [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Constructs an appropriate @scheme[dispatch-config^], invokes the @scheme[dispatch-server\@],
 and calls its @scheme[serve] function.
}
                   
@; XXX Not the right `server' above.

Here's an example of a simple web server that serves files
from a given path:

@(require (for-label web-server/dispatchers/filesystem-map)
          (prefix-in files: (for-label web-server/dispatchers/dispatch-files)))

@schemeblock[
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
                   
@defproc[(serve/ports [#:dispatch dispatch dispatcher?]
                      [#:tcp\@ tcp\@ tcp-unit^ raw:tcp\@]
                      [#:ports ports (listof integer?) (list 80)]
                      [#:listen-ip listen-ip (or/c string? false/c) #f]
                      [#:max-waiting max-waiting integer? 40]
                      [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @scheme[serve] multiple times, once for each @scheme[port], and returns
 a function that shuts down all of the server instances.
}

@defproc[(serve/ips+ports [#:dispatch dispatch dispatcher?]
                          [#:tcp\@ tcp\@ tcp-unit^ raw:tcp\@]
                          [#:ips+ports ips+ports (listof (cons/c (or/c string? false/c) (listof integer?))) (list (cons #f (list 80)))]
                          [#:max-waiting max-waiting integer? 40]
                          [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @scheme[serve/ports] multiple times, once for each @scheme[ip], and returns
 a function that shuts down all of the server instances.
}

@defproc[(do-not-return) void]{
 This function does not return. If you are writing a script to load the @web-server
 you are likely to want to call this functions at the end of your script.
}
