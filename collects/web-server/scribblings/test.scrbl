#lang scribble/doc
@(require "web-server.rkt"
          (for-label web-server/http/request-structs
                     xml
                     racket/promise
                     racket/match))

@title[#:tag "test"]{Testing Servlets}

@defmodule[web-server/test]

The @web-server provides a simple facility for writing tests for Web servlets.

The core functionality allows a request to be sent to the servlet and the response captured:

@defproc[(make-servlet-tester
          [servlet 
           (-> request?
               can-be-response?)])
         (->* ()
              ((or/c string? url? request? false/c)
               (listof binding?)
               boolean?)
              (or/c bytes?
                    xexpr?))]{
 
This function accepts 
                              
}