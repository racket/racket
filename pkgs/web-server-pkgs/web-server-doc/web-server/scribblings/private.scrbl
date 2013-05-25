#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "private" #:style 'toc]{Internal APIs}

The @web-server is a complicated piece of software and as a result,
defines a number of interesting and independently useful sub-components.
Some of these are documented here.

@local-table-of-contents[]

@include-section["timer.scrbl"]
@include-section["connection-manager.scrbl"]
@include-section["closure.scrbl"]
@include-section["cache-table.scrbl"]
@include-section["mime-types.scrbl"]
@include-section["mod-map.scrbl"]
@include-section["url-param.scrbl"]
@include-section["gzip.scrbl"]
@include-section["misc-util.scrbl"]
