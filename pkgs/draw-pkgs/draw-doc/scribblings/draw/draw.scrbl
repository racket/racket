#lang scribble/doc
@(require "common.rkt")

@title{The Racket Drawing Toolkit}

@author["Matthew Flatt" "Robert Bruce Findler" "John Clements"]

@declare-exporting[racket/draw]

@defmodule*/no-declare[(racket/draw)]{The
@racketmodname[racket/draw] library provides all of the class,
interface, and procedure bindings defined in this manual.}

@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["bitmap-class.scrbl"]
@include-section["bitmap-dc-class.scrbl"]
@include-section["brush-class.scrbl"]
@include-section["brush-list-class.scrbl"]
@include-section["color-class.scrbl"]
@include-section["color-database-intf.scrbl"]
@include-section["dc-intf.scrbl"]
@include-section["dc-path-class.scrbl"]
@include-section["font-class.scrbl"]
@include-section["font-list-class.scrbl"]
@include-section["font-name-directory-intf.scrbl"]
@include-section["gl-config-class.scrbl"]
@include-section["gl-context-intf.scrbl"]
@include-section["linear-gradient-class.scrbl"]
@include-section["pdf-dc-class.scrbl"]
@include-section["pen-class.scrbl"]
@include-section["pen-list-class.scrbl"]
@include-section["point-class.scrbl"]
@include-section["post-script-dc-class.scrbl"]
@include-section["ps-setup-class.scrbl"]
@include-section["radial-gradient-class.scrbl"]
@include-section["record-dc-class.scrbl"]
@include-section["region-class.scrbl"]
@include-section["svg-dc-class.scrbl"]
@include-section["draw-funcs.scrbl"]
@include-section["draw-contracts.scrbl"]
@include-section["draw-unit.scrbl"]
@include-section["unsafe.scrbl"]
@include-section["libs.scrbl"]

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
