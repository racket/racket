#lang scribble/doc
@(require scribble/base
          scribble/manual scribble/eval
          "utils.ss"
          (for-label unstable/debug
                     scheme/serialize
                     scheme/contract
                     scheme/base))

@title[#:tag "debug"]{Debugging}
@(define the-eval (make-base-eval))
@(the-eval '(require unstable/debug))

@defmodule[unstable/debug]

@unstable-header[]

@defform*[[(debug (f args ...))
          (debug f args ...)]]{
Produce debugging output for the application of @scheme[f], including the values of @scheme[args].
@examples[#:eval the-eval
(debug (+ 3 4 (* 5 6)))
(debug + 1 2 3)
]
}
