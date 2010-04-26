#lang scribble/doc
@(require "utils.ss")

@title{@bold{FFI}: PLT Scheme Foreign Interface}

@author["Eli Barzilay"]

@defmodule[racket/unsafe/ffi #:use-sources ('#%foreign)]

The @schememodname[racket/unsafe/ffi] library enables the direct use of
C-based APIs within Scheme programs---without writing any new C
code. From the Scheme perspective, functions and data with a C-based
API are @idefterm{foreign}, hence the term @defterm{foreign
interface}. Furthermore, since most APIs consist mostly of functions,
the foreign interface is sometimes called a @defterm{foreign function
interface}, abbreviated @deftech{FFI}.

@table-of-contents[]

@include-section["intro.scrbl"]
@include-section["libs.scrbl"]
@include-section["types.scrbl"]
@include-section["pointers.scrbl"]
@include-section["misc.scrbl"]
@include-section["derived.scrbl"]
@include-section["unexported.scrbl"]
@include-section["unsafe.scrbl"]

@index-section[]
