#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "servlet-structs.ss"]{Common Contracts}
@(require (for-label web-server/servlet/servlet-structs
                     web-server/servlet))

@defmodule[web-server/servlet/servlet-structs]{

This module provides a number of contracts
for use in servlets.

@defthing[k-url? contract?]{
Equivalent to @scheme[string?]. 
                                       
Example: @scheme["http://localhost:8080/servlets;1*1*20131636/examples/add.ss"]}

@defthing[response-generator/c contract?]{
Equivalent to @scheme[(k-url? . -> . response/c)].
           
Example: @schemeblock[(lambda (k-url)
                        `(html 
                          (body 
                           (a ([href ,k-url])
                              "Click Me to Invoke the Continuation!"))))]
}

@defthing[expiration-handler/c contract?]{
Equivalent to @scheme[(or/c false/c (request? . -> . response/c))].
           
Example: @schemeblock[(lambda (req)
                        `(html (head (title "Expired"))
                               (body (h1 "Expired")
                                     (p "This URL has expired. "
                                        "Please return to the home page."))))]
}

@defthing[embed/url/c contract?]{
Equivalent to @scheme[((request? . -> . any) . -> . string?)].

This is what @scheme[send/suspend/dispatch] gives to its function argument.
}

}
