#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "repl-module"]{The @racketmodname[racket/repl] Library}

@defmodule[racket/repl]

The @racketmodname[racket/repl] provides the same
@racket[read-eval-print-loop] binding as @racketmodname[racket/base], but
with even fewer internal dependencies than @racketmodname[racket/base]. It is
loaded in some situations on startup, as described in
@secref["init-actions"].
