#lang scribble/manual
@(require (for-label racket/base
                     syntax/quote))

@title{Preserving Source Locations}

@defmodule[syntax/quote]{The @racketmodname[syntax/quote] module
provides support for quoting syntax so that it's source locations
are preserved in marshaled bytecode form.}

@defform*[[(quote-syntax/keep-srcloc datum)
           (quote-syntax/keep-srcloc #:source source-expr datum)]]{

Like @racket[(quote-syntax datum)], but the source locations of
@racket[datum] are preserved. If a @racket[source-expr] is provided,
then it is used in place of a @racket[syntax-source] value for
each syntax object within @racket[datum].

Unlike a @racket[quote-syntax] form, the results of evaluating the
expression multiple times are not necessarily @racket[eq?].}
