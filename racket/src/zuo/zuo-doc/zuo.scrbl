#lang scribble/manual

@title{Zuo: A Tiny Racket for Scripting}

You should use Racket to write scripts. But for the case where you
need something much smaller than Racket for some reason, or the case
you're trying to script the build of Racket itself, Zuo is a tiny
Racket with primitives for dealing with files and running processes.

@table-of-contents[]

@include-section["overview.scrbl"]
@include-section["lang-zuo.scrbl"]
@include-section["zuo-build.scrbl"]
@include-section["zuo-lib.scrbl"]
@include-section["lang-zuo-hygienic.scrbl"]
@include-section["lang-zuo-datum.scrbl"]
@include-section["lang-zuo-kernel.scrbl"]
@include-section["reader.scrbl"]
