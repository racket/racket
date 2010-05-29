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
@(the-eval '(require unstable/debug racket/match))

@defmodule[unstable/debug]

@unstable-header[]

@defform*[[(debug/call (f args ...))
          (debug/call f args ...)]]{
Produce debugging output for the application of @racket[f], including the values of @racket[args].
@examples[#:eval the-eval
(debug/call (+ 3 4 (* 5 6)))
(debug/call + 1 2 3)
]
}

@defform*[[(debug/macro f args ...)]]{
Produce debugging output for the application of @racket[f], but does
not parse or print args.  Suitable for use debugging macros.
@examples[#:eval the-eval
(debug/macro match (list 1 2 3)
  [(list x y z) (+ x y z)])
(debug/macro + 1 2 3)
]
}
