#lang scribble/doc
@(require "common.ss")

@title{@bold{GUI}: Racket Graphical Interface Toolkit}

@author["Matthew Flatt" "Robert Bruce Findler" "John Clements"]

@declare-exporting[racket/gui/base racket/gui #:use-sources (mred)]

@defmodule*/no-declare[(racket/gui/base)]{The
@racketmodname[racket/gui/base] library provides all of the class,
interface, and procedure bindings defined in this manual, in addition
to the bindings of @racketmodname[racket/draw].}

@defmodulelang*/no-declare[(racket/gui)]{The
@racketmodname[racket/gui] language combines all bindings of the
@racketmodname[racket] language and the
@racketmodname[racket/gui/base] and @racketmodname[racket/draw] modules.}


@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@include-section["prefs.scrbl"]
@include-section["dynamic.scrbl"]

@;------------------------------------------------------------------------

@index-section[]
