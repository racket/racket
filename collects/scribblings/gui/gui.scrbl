#lang scribble/doc
@(require "common.ss")

@title{@bold{GUI}: Racket Graphics Toolkit}

@author["Matthew Flatt" "Robert Bruce Findler" "John Clements"]

@declare-exporting[racket/gui/base racket/gui #:use-sources (mred)]

@defmodule*/no-declare[(racket/gui/base)]{The
@racketmodname[racket/gui/base] library provides all of the class,
interface, and procedure bindings defined in this manual.}

@defmodulelang*/no-declare[(racket/gui)]{The
@racketmodname[racket/gui] language combines all bindings of the
@racketmodname[racket] language and the
@racketmodname[racket/gui/base] modules.}


@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@include-section["config.scrbl"]
@include-section["dynamic.scrbl"]


@;------------------------------------------------------------------------

@(bibliography

  (bib-entry #:key "Adobe99"
             #:author "Adobe Systems Incorporated"
             #:title "PostScript Language Reference, third edition"
             #:is-book? #t
             #:url "http://partners.adobe.com/public/developer/en/ps/PLRM.pdf"
             #:date "1999")

  )

@;------------------------------------------------------------------------

@index-section[]
