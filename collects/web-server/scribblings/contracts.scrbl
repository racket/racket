#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "servlet-structs"]{Common Contracts}
@(require (for-label web-server/servlet/servlet-structs
                     web-server/http
                     web-server/servlet))

@defmodule[web-server/servlet/servlet-structs]{

This module provides a number of contracts
for use in servlets.

@defparam[current-response/c ctc contract?]{
The contract used by @racket[response/c] dynamically. Defaults to @racket[any/c].
}

@defthing[response/c contract?]{
A contract corresponding to @racket[(dynamic/c any/c current-response/c response?)].
                            
This allows Web applications to customize the Web Server's handling of responses, while ensuring that the Web Server
always receives @racket[response?] structures.
}                           

@defthing[k-url? contract?]{
Equivalent to @racket[string?]. 
                                       
Example: @racket["http://localhost:8080/servlets;1*1*20131636/examples/add.rkt"]}

@defthing[response-generator/c contract?]{
Equivalent to @racket[(k-url? . -> . response/c)].
           
Example: @racketblock[(lambda (k-url)
                        (response/xexpr
                         `(html 
                           (body 
                            (a ([href ,k-url])
                               "Click Me to Invoke the Continuation!")))))]
}

@defthing[expiration-handler/c contract?]{
Equivalent to @racket[(or/c false/c (request? . -> . response/c))].
              
Typically @racket[#f] uses the default expiration handler, which displays an error message.
           
Example: @racketblock[(lambda (req)
                        (response/xexpr
                         `(html (head (title "Expired"))
                                (body (h1 "Expired")
                                      (p "This URL has expired. "
                                         "Please return to the home page.")))))]
}

@defthing[embed/url/c contract?]{
Equivalent to @racket[((request? . -> . any) . -> . string?)].

This is what @racket[send/suspend/dispatch] gives to its function argument.
}

}
