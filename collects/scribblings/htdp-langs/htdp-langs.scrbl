#lang scribble/doc
@(require "common.rkt" (for-label lang/htdp-beginner))

@title[#:style 'toc #:tag "top"]{@italic{How to Design Programs} Languages}

The languages documented in this manual are provided by DrRacket to be
used with the @italic{@link["http://www.htdp.org/"]{How to Design
Programs}} book.

When programs in these languages are run in DrRacket, any part of the
program that was not run is highlighted in orange and black. These
colors are intended to give the programmer feedback about the parts of
the program that have not been tested. To avoid seeing these colors,
use @racket[check-expect] to test your program. Of course, just
because you see no colors, does not mean that your program has been
fully tested; it simply means that each part of the program has been run
(at least once).

@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["beginner.scrbl"]
@include-section["beginner-abbr.scrbl"]
@include-section["intermediate.scrbl"]
@include-section["intermediate-lambda.scrbl"]
@include-section["advanced.scrbl"]

@;------------------------------------------------------------------------

@;index-section[]
