#lang scribble/doc
@(require "web-server.ss")

@title[#:style 'toc #:tag "module-servlets"]{Defining a Servlet}

A @defterm{servlet} is a module with particular exports. There three kinds of servlets.

@local-table-of-contents[]

@include-section["v1-servlet.scrbl"]
@include-section["v2-servlet.scrbl"]
@include-section["stateless-servlet.scrbl"]
