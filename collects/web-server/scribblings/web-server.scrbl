#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "web-server-ref"]{@bold{Web}: PLT Web Applications}

@author{Jay McCarthy}

This manual describes the PLT libraries for building Web applications.

@secref["run.ss"] describes how to run the servlets you've written. 

@secref["servlet"] and @secref["stateless"] describe two ways to write Web applications. 
@secref["servlet"] use the entire PLT Scheme language, but their continuations are stored in the Web server's memory.
@secref["stateless"] use a slightly restricted PLT Scheme language, but their continuation can be stored by the Web client or on a Web server's disk. If you can, you want to use @secref["stateless"] for the improved scalability.

The @secref["http"] section describes the common library function for manipulating HTTP requests and creating HTTP responses.
In particular, this section covers cookies, authentication, and request bindings.

The final three sections (@secref["dispatch"], @secref["formlets"], and @secref["templates"]) cover utility libraries that
ease the creation of typical Web applications.

This manual closes with a frequently asked questions section: @secref["faq"].

@local-table-of-contents[]

@include-section["running.scrbl"]

@include-section["servlet.scrbl"]
@include-section["lang-api.scrbl"]

@include-section["http.scrbl"]

@include-section["dispatch.scrbl"]
@include-section["formlets.scrbl"]
@include-section["templates.scrbl"]

@include-section["faq.scrbl"]

@index-section[]
