#lang scribble/manual

@(require "utils.rkt")

@title[#:tag "top"]{Math Library}
@(author-neil)
@(author-jens-axel)

@defmodule[math]

The @racketmodname[math] library provides functions and data structures useful
for working with numbers and collections of numbers. These include
@itemlist[
  @item{Additional constants and elementary functions}
  @item{Special functions}
  @item{@racket[Bigfloat]s, or arbitrary-precision floating-point numbers}
  @item{Number-theoretic functions}
  @item{@racket[Array]s for storing and transforming large rectangular data sets}
  @item{Probability distributions}
  @item{Statistical functions (currently undergoing refactoring)}
  @item{Linear algebra functions (currently under review)}
]

With this library, we hope to support a wide variety of applied mathematics in
Racket, including simulation, statistical inference, signal processing, and
combinatorics. If you find it lacking for your variety of mathematics,
please contact us or post to one of the
@hyperlink["http://racket-lang.org/community.html"]{mailing lists}
to make suggestions or submit patches.

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
@include-section["math-statistics.scrbl"]
@include-section["math-distributions.scrbl"]
@include-section["math-utils.scrbl"]
