#lang scribble/manual

@(require "utils.rkt")

@title[#:tag "top"]{Math Library}
@(author-neil)
@(author-jens-axel)

@defmodule[math]

The @racketmodname[math] library provides functions and data structures useful
for working with numbers and collections of numbers. These include
@itemlist[
  @item{@racketmodname[math/base]: Constants and elementary functions}
  @item{@racketmodname[math/flonum]: Flonum functions, including high-accuracy support}
  @item{@racketmodname[math/special-functions]: Special (i.e. non-elementary) functions}
  @item{@racketmodname[math/bigfloat]: Arbitrary-precision floating-point functions}
  @item{@racketmodname[math/number-theory]: Number-theoretic functions}
  @item{@racketmodname[math/array]: Functional arrays for operating on large rectangular data sets}
  @item{@racketmodname[math/matrix]: Linear algebra functions for arrays}
  @item{@racketmodname[math/distributions]: Probability distributions}
  @item{@racketmodname[math/statistics]: Statistical functions}
]

With this library, we hope to support a wide variety of applied mathematics in
Racket, including simulation, statistical inference, signal processing, and
combinatorics. If you find it lacking for your variety of mathematics, please
@itemlist[@item{Visit the @hyperlink["https://github.com/plt/racket/wiki/Math-Library-Features"]{Math Library Features wiki page}
                to see what is planned.}
          @item{Contact us or post to one of the @hyperlink["http://racket-lang.org/community.html"]{mailing lists} to
                make suggestions or submit patches.}]

@bold{This is a Typed Racket library.} It is most efficient to use it in Typed
Racket, so that contracts are checked statically. However, almost all of it can
be used in untyped Racket. Exceptions and performance warnings are in @bold{bold text}.

@local-table-of-contents[]

@include-section["math-base.scrbl"]
@include-section["math-flonum.scrbl"]
@include-section["math-special-functions.scrbl"]
@include-section["math-number-theory.scrbl"]
@include-section["math-bigfloat.scrbl"]
@include-section["math-array.scrbl"]
@include-section["math-matrix.scrbl"]
@include-section["math-statistics.scrbl"]
@include-section["math-distributions.scrbl"]
@include-section["math-utils.scrbl"]
