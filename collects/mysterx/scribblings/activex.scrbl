#lang scribble/doc
@(require "common.rkt")

@title[#:tag "activex" #:style 'toc]{ActiveX and DHTML}

A MysterX application consists of one or more browsers, which are
instances of the class @racket[mx-browser%].

@(deprecated)

@local-table-of-contents[]

@include-section["browsers.scrbl"]
@include-section["documents.scrbl"]
@include-section["html-events.scrbl"]
@include-section["html.scrbl"]
