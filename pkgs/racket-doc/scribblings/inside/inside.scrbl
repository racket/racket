#lang scribble/doc
@(require "utils.rkt")

@title[#:tag-prefix '(lib "scribblings/inside/inside.scrbl") 
       #:tag "top"]{Inside: Racket C API}

@author["Matthew Flatt"]

The Racket runtime system is responsible for the implementation of
primitive datatypes such as numbers and strings, the macro expansion
and compilation of Racket from source, the allocation and reclamation
of memory used during evaluation, and the scheduling of concurrent
threads and parallel tasks.

This manual describes the C interface of Racket's runtime system,
which varies depending on the implementation of Racket (see
@secref[#:doc '(lib "scribblings/guide/guide.scrbl")
"virtual-machines"]): the CS implementation of Racket has one
interface, while the BC (3m and CGC) implementation of Racket has
another.

The C interface is relevant to some degree when interacting with
foreign libraries as described in @other-manual['(lib
"scribblings/foreign/foreign.scrbl")]. Even though interactions with
foreign code are constructed in pure Racket using the
@racketmodname[ffi/unsafe] module, many details of representations,
memory management, and concurrency are described here. This manual
also describes embedding the Racket run-time system in larger programs
and extending Racket directly with C-implemented libraries.

@table-of-contents[]

@; ------------------------------------------------------------------------

@include-section["cs.scrbl"]
@include-section["bc.scrbl"]
@include-section["appendix.scrbl"]

@; ------------------------------------------------------------------------

@index-section[]
