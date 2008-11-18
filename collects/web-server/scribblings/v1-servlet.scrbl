#lang scribble/doc
@(require "web-server.ss")

@title{Version 1 Servlets}
@(require (for-label web-server/http
                     "dummy-v1-servlet.ss")) @; to give a binding context
@declare-exporting[#:use-sources (web-server/scribblings/dummy-v1-servlet)]

@defthing[interface-version (one-of/c 'v1)]{
 This indicates that the servlet is a version one servlet.
}

@defthing[timeout integer?]{
 This number is used as the @scheme[continuation-timeout] argument to
 a timeout-based continuation manager used for this servlet. (See
 @secref["timeouts.ss"].) (i.e., you do not have a choice of the manager
 for this servlet and will be given a timeout-based manager.)
}

@defproc[(start [initial-request request?])
         response?]{
 This function is called when an instance of this servlet is started.
 The argument is the HTTP request that initiated the instance.
}
                   
An example version 1 module:
@schememod[
 scheme
 
 (define interface-version 'v1)
 (define timeout (* 60 60 24))
 (define (start req)
   `(html (head (title "Hello World!"))
          (body (h1 "Hi Mom!"))))
 ]

These servlets should use the @schememodname[web-server/servlet] API.