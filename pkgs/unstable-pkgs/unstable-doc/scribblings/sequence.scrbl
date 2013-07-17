#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket/base unstable/sequence racket/contract))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/sequence))
@(define-syntax speed
    (syntax-rules ()
      [(_ id what)
       (t "An " (racket id) " application can provide better performance for "
          (elem what)
          " iteration when it appears directly in a " (racket for)
          " clause.")]))

@title[#:tag "sequence"]{Sequences}
@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]

@defmodule[unstable/sequence]


@defproc[(in-syntax [stx syntax?]) sequence?]{
Produces a sequence equivalent to @racket[(syntax->list lst)].
@speed[in-syntax "syntax"]

@examples[#:eval the-eval
(for/list ([x (in-syntax #'(1 2 3))])
  x)]}

@defproc[(in-pairs [seq sequence?]) sequence?]{
Produces a sequence equivalent to
 @racket[(in-parallel (sequence-lift car seq) (sequence-lift cdr seq))].
}

@defproc[(in-sequence-forever [seq sequence?] [val any/c]) sequence?]{
Produces a sequence whose values are the elements of @racket[seq], followed by @racket[val] repeated.
}

@defproc[(sequence-lift [f procedure?] [seq sequence?]) sequence?]{
Produces the sequence of @racket[f] applied to each element of @racket[seq].
@examples[#:eval the-eval
(for/list ([x (sequence-lift add1 (in-range 10))])
  x)]
}


@addition{David Vanderson}

@defproc[(in-slice [length exact-positive-integer?] [seq sequence?])
         sequence?]{
  Returns a sequence where each element is a list with @racket[length]
  elements from the given sequence.

  @examples[#:eval the-eval
  (for/list ([e (in-slice 3 (in-range 8))]) e)
  ]
}


@close-eval[the-eval]
