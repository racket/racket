#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/port))

@mzlib[#:mode title port]

@deprecated[@racketmodname[racket/port]]{}

The @racketmodname[mzlib/port] library mostly re-provides
@racketmodname[racket/port].

@defproc[(strip-shell-command-start [in input-port?]) void?]{

Reads and discards a leading @litchar{#!} in @racket[in] (plus
continuing lines if the line ends with a backslash). Since
@litchar{#!}  followed by a forward slash or space is a comment, this
procedure is not needed before reading Scheme expressions.}

