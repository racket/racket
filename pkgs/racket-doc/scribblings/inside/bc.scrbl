#lang scribble/base

@title[#:style '(toc grouper) #:tag "bc"]{Inside Racket BC (3m and CGC)}

The Racket BC API was originally designed for a tight integration with
C code. As a result, the BC API is considerably larger than the Racket
CS API.

@local-table-of-contents[]

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
