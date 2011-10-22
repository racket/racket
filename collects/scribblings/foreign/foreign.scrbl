#lang scribble/doc
@(require "utils.rkt")

@title{The Racket Foreign Interface}

@author["Eli Barzilay"]

@defmodule[ffi/unsafe #:use-sources ('#%foreign)]

The @racketmodname[ffi/unsafe] library enables the direct use of
C-based APIs within Racket programs---without writing any new C
code. From the Racket perspective, functions and data with a C-based
API are @idefterm{foreign}, hence the term @defterm{foreign
interface}. Furthermore, since most APIs consist mostly of functions,
the foreign interface is sometimes called a @defterm{foreign function
interface}, abbreviated @deftech{FFI}.

@;------------------------------------------------------------------------

@table-of-contents[]

@include-section["intro.scrbl"]
@include-section["libs.scrbl"]
@include-section["types.scrbl"]
@include-section["pointers.scrbl"]
@include-section["derived.scrbl"]
@include-section["misc.scrbl"]
@include-section["unexported.scrbl"]

@(bibliography
  (bib-entry #:key "Barzilay04"
             #:author "Eli Barzilay and Dmitry Orlovsky"
             #:title "Foreign Interface for PLT Scheme"
             #:location "Workshop on Scheme and Functional Programming"
             #:date "2004"))

@index-section[]
