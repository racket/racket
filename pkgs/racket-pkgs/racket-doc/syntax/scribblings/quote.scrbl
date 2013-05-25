#lang scribble/manual
@(require (for-label racket/base
                     syntax/quote))

@title{Preserving Source Locations}

@defmodule[syntax/quote]{The @racketmodname[syntax/quote] module
provides support for quoting syntax so that it's source locations
are preserved in marshaled bytecode form.}

@defform[(quote-syntax/keep-srcloc datum)]{

Like @racket[(quote-syntax datum)], but the source locations of
@racket[datum] are preserved.

Unlike a @racket[quote-syntax] form, the results of evaluating the
expression multiple times are not necessarily @racket[eq?].}
