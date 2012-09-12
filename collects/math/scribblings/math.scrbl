#lang scribble/manual

@(define (neil-email) "ntoronto@racket-lang.org")
@(define (jens-axel-email) "jensaxel@soegaard.net")

@title[#:tag "top"]{Math Library}
@author{@(author+email "Neil Toronto" (neil-email))}
@author{@(author+email "Jens Axel Søgaard" (jens-axel-email))}

@defmodule[math]

The @racketmodname[math] library provides functions and data structures useful
for working with integers, real numbers, and collections of such. These include
@itemlist[
  @item{Additional floating-point functions}
  @item{Probability distributions}
  @item{Number-theoretic functions}
  @item{Polynomials}
  @item{Intervals}
  @item{@racket[Bigfloat]s, or arbitrary-precision floating-point numbers}
  @item{@racket[Array]s for storing and transforming large rectangular data sets}
  @item{Linear algebra functions}
]

With this library, we hope to support a wide variety of applied mathematics in
Racket, including simulation, statistical inference, signal processing, and
combinatorics. If you find it lacking for your variety of mathematics,
please contact us or post to one of the
@hyperlink["http://racket-lang.org/community.html"]{mailing lists}
to make suggestions or submit patches.

@bold{This is a Typed Racket library.} A large portion of it can be used from
untyped Racket, and we are working on making the rest available. Unavailable
exports are noted @bold{in bold text}.

@local-table-of-contents[]

@include-section["number-theory.scrbl"]
@include-section["flonum.scrbl"]
