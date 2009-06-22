#lang scribble/doc
@(require "web-server.ss")

@title[]{Simple Single Servlet Servers}
@(require (for-label web-server/servlet-env
                     web-server/servlet-dispatch
                     web-server/http
                     web-server/managers/lru
                     web-server/private/util
                     web-server/dispatchers/dispatch
                     web-server/configuration/configuration-table
                     web-server/configuration/responders
                     web-server/dispatchers/dispatch-log
                     scheme/serialize
                     web-server/stuffers
                     scheme/list))

@defmodule[web-server/servlet-dispatch]{

These functions optimize the construction of dispatchers and launching of servers for single servlets and interactive development.

@defproc[(dispatch/servlet 
          [start (request? . -> . response/c)]
          [#:regexp regexp regexp? #rx""]
          [#:stateless? stateless? boolean? #f]
          [#:stuffer stuffer (stuffer/c serializable? bytes?) default-stuffer]
          [#:manager manager manager? (make-threshold-LRU-manager #f (* 1024 1024 64))]
          [#:namespace namespace (listof module-path?) empty]
          [#:current-directory servlet-current-directory path-string? (current-directory)])
         dispatcher/c]{
 @scheme[serve/servlet] starts a server and uses a particular dispatching sequence. For some applications, this
 nails down too much, but users are conflicted, because the interface is so convenient. For those users, @scheme[dispatch/servlet]
 does the hardest part of @scheme[serve/servlet] and constructs a dispatcher just for the @scheme[start] servlet.
 
 The dispatcher responds to requests that match @scheme[regexp]. The current directory
 of servlet execution is @scheme[servlet-current-directory]. 
 
 If @scheme[stateless?] is true, then the servlet is run as a stateless @schememod[web-server] module and @scheme[stuffer] is used
 as the @tech[#:doc '(lib "web-server/scribblings/web-server.scrbl")]{stuffer}.

 The servlet is loaded with @scheme[manager] as its continuation manager. (The default manager limits the amount of memory to 64 MB and
 deals with memory pressure as discussed in the @scheme[make-threshold-LRU-manager] documentation.)
 
 The modules specified by @scheme[servlet-namespace] are shared with other servlets.
}
                      
@defproc[(serve/launch/wait
          [make-dispatcher (semaphore? . -> . dispatcher/c)]
          [#:launch-path launch-path (or/c false/c string?) #f]
          [#:banner? banner? boolean? #f]
          [#:listen-ip listen-ip (or/c false/c string?) "127.0.0.1"]
          [#:port port number? 8000]
          [#:ssl-cert ssl-cert (or/c false/c path-string?) #f]
          [#:ssl-key ssl-key (or/c false/c path-string?) #f])
         void]{
 The other interesting part of @scheme[serve/servlet] is its ability to start up a server and immediately
 launch a browser at it. This is provided by @scheme[serve/launch/wait].
 
 It starts a server using the result of @scheme[make-dispatcher] as the dispatcher. @scheme[make-dispatcher] is supplied
 a semaphore that if posted, will cause the server to quit.
 
 If @scheme[launch-path] is not false, then a browser is launched with that path appended to the URL to the server itself.
 
 If @scheme[banner?] is true, then a banner is printed informing the user of the server's URL.
 
 The server listens on @scheme[listen-ip] and port @scheme[port].
 
 If @scheme[ssl-key] and @scheme[ssl-cert] are not false, then the server runs in HTTPS mode with @scheme[ssl-cert]
 and @scheme[ssl-key] as paths to the certificate and private key.    
}
              
}
