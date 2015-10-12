#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/os))

@title[#:tag "os-lib"]{Additional Operating System Functions}

@defmodule[racket/os]{The @racketmodname[racket/os] library
  additional functions for querying the operating system.}

@history[#:added "6.3"]

@defproc[(gethostname) string?]{
  Returns a string for the current machine's hostname (including its domain).
}

@defproc[(getpid) exact-integer?]{
  Returns an integer identifying the current process within the operating system.
}
