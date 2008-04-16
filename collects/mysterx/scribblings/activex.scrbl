#lang scribble/doc
@(require "common.ss")

@title[#:tag "activex" #:style 'toc]{ActiveX and DHTML}

A MysterX application consists of one or more browsers, which are
instances of the class @scheme[mx-browser%].

@local-table-of-contents[]

@include-section["browsers.scrbl"]
@include-section["documents.scrbl"]
@include-section["html-events.scrbl"]
@include-section["html.scrbl"]
