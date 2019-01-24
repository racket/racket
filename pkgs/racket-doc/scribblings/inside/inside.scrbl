#lang scribble/doc
@(require "utils.rkt")

@title[#:tag-prefix '(lib "scribblings/inside/inside.scrbl") 
       #:tag "top"]{Inside: Racket C API}

@author["Matthew Flatt"]

This manual describes the C interface of Racket's runtime system for
the 3m and CGC variants of Racket (but not the CS variant; see
@secref[#:doc '(lib "scribblings/guide/guide.scrbl")
"virtual-machines"]). The C
interface is relevant primarily when interacting with foreign
libraries as described in @other-manual['(lib
"scribblings/foreign/foreign.scrbl")]; even though interactions with
foreign code are constructed in pure Racket using the
@racketmodname[ffi/unsafe] module, many details of representations,
memory management, and concurrency are described here. This manual
also describes embedding the Racket run-time system in larger programs
and extending Racket directly with C-implemented libraries.

@table-of-contents[]

@; ------------------------------------------------------------------------

@include-section["overview.scrbl"]
@include-section["embedding.scrbl"]
@include-section["extensions.scrbl"]
@include-section["values.scrbl"]
@include-section["memory.scrbl"]
@include-section["namespaces.scrbl"]
@include-section["procedures.scrbl"]
@include-section["eval.scrbl"]
@include-section["exns.scrbl"]
@include-section["threads.scrbl"]
@include-section["params.scrbl"]
@include-section["contmarks.scrbl"]
@include-section["strings.scrbl"]
@include-section["numbers.scrbl"]
@include-section["ports.scrbl"]
@include-section["structures.scrbl"]
@include-section["security.scrbl"]
@include-section["custodians.scrbl"]
@include-section["subprocesses.scrbl"]
@include-section["misc.scrbl"]

@; ------------------------------------------------------------------------

@index-section[]
