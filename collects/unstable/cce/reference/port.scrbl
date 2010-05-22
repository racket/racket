#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/port))

@title[#:style 'quiet #:tag "cce-port"]{Ports}

@defmodule[unstable/cce/port]

This module provides tools for port I/O.

@defproc[(eprintf [fmt string?] [arg any/c] ...) void?]{

Like @scheme[printf], but prints to @scheme[(current-error-port)].

@defexamples[
#:eval (evaluator 'unstable/cce/port)
(eprintf "Danger, ~a!" "Will Robinson")
]

}

@defproc[(read-all [reader (-> any/c) read]
                   [port input-port? (current-input-port)])
         list?]{

This function produces a list of all the values produced by calling
@scheme[(reader)] while @scheme[current-input-port] is set to @scheme[port], up
until it produces @scheme[eof].

@defexamples[
#:eval (evaluator 'unstable/cce/port)
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
#:eval (evaluator 'unstable/cce/port)
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
#:eval (evaluator 'unstable/cce/port)
(define port (open-input-string "1 2 3"))
(port-count-lines! port)
(read port)
(port->srcloc port)
(port->srcloc port "1 2 3" 1)
]

}

@defproc[(read-available-bytes [port input-port? (current-input-port)])
         (or/c bytes? eof-object?)]{

This function reads all immediately available bytes from a port and produces a
byte string containing them.  If there are no bytes available and the port is
known to have no more input, it produces @scheme[eof]; if there are none
available but the port may have more input, it produces an empty byte string.
This procedure never blocks to wait for input from the port.

@defexamples[
#:eval (evaluator 'unstable/cce/port)
(define-values [in out] (make-pipe))
(parameterize ([current-input-port in]) (read-available-bytes))
(write-byte (char->integer #\c) out)
(read-available-bytes in)
(read-available-bytes in)
(close-output-port out)
(read-available-bytes in)
]

}
