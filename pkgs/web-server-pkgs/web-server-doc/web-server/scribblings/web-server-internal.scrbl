#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "web-server-ref-internal"]{Web Server: HTTP Server}

@author{Jay McCarthy}

This manual describes the internals of the Racket Web Server.

@local-table-of-contents[]

@include-section["dispatch-server.scrbl"]
@include-section["dispatchers.scrbl"]
@include-section["launch.scrbl"]
@include-section["web-config.scrbl"]
@include-section["private.scrbl"]
@include-section["server-faq.scrbl"]

@index-section[]
