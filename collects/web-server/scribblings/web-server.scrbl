#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-server-ref"]{Web Applications in Racket}

@author{Jay McCarthy}

This manual describes the Racket libraries for building Web applications.

@secref["run"] describes how to run the servlets you've written. 

@secref["servlet"] and @secref["stateless"] describe two ways to write Web applications. 
@secref["servlet"] use the entire Racket language, but their continuations are stored in the Web server's memory.
@secref["stateless"] use a slightly restricted Racket language, but their continuation can be stored by the Web client or on a Web server's disk. If you can, you want to use @secref["stateless"] for the improved scalability.

The @secref["http"] section describes the common library functions for manipulating HTTP requests and creating HTTP responses.
In particular, this section covers cookies, authentication, and request bindings.

The final five sections (@secref["dispatch"], @secref["formlets"], @secref["templates"], @secref["page"], and @secref["test"]) cover utility libraries that ease the creation of typical Web applications.

This manual closes with a frequently asked questions section: @secref["faq"].

@local-table-of-contents[]

@include-section["running.scrbl"]

@include-section["servlet.scrbl"]
@include-section["lang-api.scrbl"]

@include-section["http.scrbl"]

@include-section["dispatch.scrbl"]
@include-section["formlets.scrbl"]
@include-section["templates.scrbl"]
@include-section["page.scrbl"]

@include-section["test.scrbl"]

@include-section["faq.scrbl"]

@index-section[]
