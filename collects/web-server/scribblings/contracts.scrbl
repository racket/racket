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

}
