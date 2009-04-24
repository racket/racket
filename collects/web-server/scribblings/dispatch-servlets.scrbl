#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "dispatch-servlets.ss"]{Serving Servlets}
@a-dispatcher[web-server/dispatchers/dispatch-servlets
              @elem{defines a dispatcher constructor
                    that runs servlets.}]{
          
@defthing[url->servlet/c contract?]{Equivalent to @scheme[(url? . -> . servlet?)]}

@defproc[(make-cached-url->servlet
          [url->path url->path/c]
          [path->serlvet path->servlet/c])         
         (values (-> void)
                 url->servlet/c)]{
 The first return value flushes the cache. 
 The second is a procedure that uses @scheme[url->path] to resolve the URL to a path, then uses @scheme[path->servlet] to resolve
 that path to a servlet, caching the results in an internal table.
}
                        
@defproc[(make [url->servlet url->servlet/c]
               [#:responders-servlet-loading
                responders-servlet-loading
                (url? exn? . -> . response/c)
                servlet-loading-responder]
               [#:responders-servlet
                responders-servlet
                (url? exn? . -> . response/c)
                servlet-error-responder])
         dispatcher/c]{
 This dispatcher runs Scheme servlets, using @scheme[url->servlet] to resolve URLs to the underlying servlets.
 If servlets have errors loading, then @scheme[responders-servlet-loading] is used. Other errors are handled with
 @scheme[responders-servlet]. If a servlet raises calls @scheme[next-dispatcher], then the signal is propagated by this dispatcher.
}
                      
}

@include-section["servlet-setup.scrbl"]

@include-section["namespace.scrbl"]

@section{Internal Servlet Representation}

@defmodule[web-server/private/servlet]{                                       
 @defstruct[servlet ([custodian custodian?]
                     [namespace namespace?]
                     [manager manager?]
                     [directory path-string?]
                     [handler (request? . -> . response/c)])
                    #:mutable]{
  Instances of this structure hold the necessary parts of a servlet:
  the @scheme[custodian] responsible for the servlet's resources,
  the @scheme[namespace] the servlet is executed within,
  the @scheme[manager] responsible for the servlet's continuations,
  the current @scheme[directory] of the servlet,
  and the @scheme[handler] for all requests to the servlet.
 }
}
