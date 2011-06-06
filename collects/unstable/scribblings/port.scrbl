#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/port))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/port))

@title{Ports}

@defmodule[unstable/port]

This module provides tools for port I/O.

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defproc[(read-all [reader (-> any/c) read]
                   [port input-port? (current-input-port)])
         list?]{

This function produces a list of all the values produced by calling
@scheme[(reader)] while @scheme[current-input-port] is set to @scheme[port], up
until it produces @scheme[eof].

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
objects produced by calling @scheme[(reader)] while @scheme[current-input-port]
is set to @scheme[port], up until it produces @scheme[eof].  The source location
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

@defproc[(port->srcloc [port port?]
                       [source any/c (object-name port)]
                       [span exact-nonnegative-integer? 0])
         srcloc?]{

Produces a @scheme[srcloc] structure representing the current position of a
port, using the provided @scheme[source] and @scheme[span] values to fill in
missing fields.  This function relies on @scheme[port-next-location], so line
counting must be enabled for @scheme[port] to get meaningful results.

@defexamples[
#:eval the-eval
(define port (open-input-string "1 2 3"))
(port-count-lines! port)
(read port)
(port->srcloc port)
(port->srcloc port "1 2 3" 1)
]

}

@(close-eval the-eval)
