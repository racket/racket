#lang scribble/doc
@(require scribble/base
          scribble/manual scribble/eval
          "utils.rkt"
          (for-label unstable/debug
                     racket/serialize
                     racket/contract
                     racket/base))

@title[#:tag "debug"]{Debugging}
@(define the-eval (make-base-eval))
@(the-eval '(require unstable/debug))

@defmodule[unstable/debug]

@unstable-header[]

@defform*[[(debug (f args ...))
          (debug f args ...)]]{
Produce debugging output for the application of @racket[f], including the values of @racket[args].
@examples[#:eval the-eval
(debug (+ 3 4 (* 5 6)))
(debug + 1 2 3)
]
}
