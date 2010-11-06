#lang scribble/doc
@(require "common.ss")

@title{@bold{Draw}: Racket Drawing Toolkit}

@author["Matthew Flatt" "Robert Bruce Findler" "John Clements"]

@declare-exporting[racket/draw]

@defmodule*/no-declare[(racket/draw)]{The
@racketmodname[racket/draw] library provides all of the class,
interface, and procedure bindings defined in this manual.}

@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]

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
