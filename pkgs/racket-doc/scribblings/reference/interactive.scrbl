#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "interactive"]{Interactive Module Loading}

The @racketmodname[racket/rerequire] and @racketmodname[racket/enter]
libraries provide support for loading, reloading, and using modules.

@include-section["enter.scrbl"]

@include-section["rerequire.scrbl"]
