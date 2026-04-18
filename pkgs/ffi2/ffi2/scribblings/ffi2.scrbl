#lang scribble/manual

@title{FFI2: Racket Foreign Interface}

@defmodule[ffi2]{The @racketmodname[ffi2] library is an alternative to
@racketmodname[ffi/unsafe] @margin-note*{For more information about the
motivation for @racketmodname[ffi2], see @secref["interop"].} for
using libraries that have a C-based API---without writing any new C
code. From the Racket perspective, functions and data with a C-based
API are @idefterm{foreign}, hence the term @defterm{foreign
interface}. Furthermore, since most APIs consist mostly of functions,
the foreign interface is sometimes called a @defterm{foreign function
interface}, abbreviated @deftech{FFI}.}

@table-of-contents[]

@include-section["overview.scrbl"]
@include-section["api.scrbl"]
@include-section["interop.scrbl"]
