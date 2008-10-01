#lang scribble/doc
@(require "common.ss")

@title{@bold{GUI}: PLT Graphics Toolkit}

@author["Matthew Flatt" "Robert Bruce Findler" "John Clements"]

@declare-exporting[scheme/gui/base scheme/gui #:use-sources (mred)]

This reference manual describes the GUI toolbox that is part of PLT
 Scheme and whose core is implemented by the MrEd executable.

@defmodule*/no-declare[(scheme/gui/base)]{The
@schememodname[scheme/gui/base] library provides all of the class,
interface, and procedure bindings defined in this manual. At run time,
this library needs primitive graphics support that the MrEd executable
provides; this library cannot run in MzScheme.}

@defmodulelang*/no-declare[(scheme/gui)]{The
@schememodname[scheme/gui] language combines all bindings of the
@schememodname[scheme] language and the
@schememodname[scheme/gui/base] modules.}


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
