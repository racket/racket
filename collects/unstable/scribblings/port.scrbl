#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/port))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/port))

@title{Ports}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/port]

This module provides tools for port I/O.

@defproc[(read-all [reader (-> any/c) read]
                   [port input-port? (current-input-port)])
         list?]{

This function produces a list of all the values produced by calling
@racket[(reader)] while @racket[current-input-port] is set to @racket[port], up
until it produces @racket[eof].

@defexamples[
#:eval the-eval
(read-all read (open-input-string "1 2 3"))
(parameterize ([current-input-port (open-input-string "a b c")])
  (read-all))
]

}

@defproc[(read-all-syntax [reader (-> (or/c syntax? eof-object?)) read]
                          [port input-port? (current-input-port)])
         (syntax/c list?)]{

This function produces a syntax object containing a list of all the syntax
objects produced by calling @racket[(reader)] while @racket[current-input-port]
is set to @racket[port], up until it produces @racket[eof].  The source location
of the result spans the entire portion of the port that was read.

@defexamples[
#:eval the-eval
(define port1 (open-input-string "1 2 3"))
(port-count-lines! port1)
(read-all-syntax read-syntax port1)
(define port2 (open-input-string "a b c"))
(port-count-lines! port2)
(parameterize ([current-input-port port2])
  (read-all-syntax))
]

}

@(close-eval the-eval)
