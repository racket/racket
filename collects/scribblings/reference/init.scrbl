#lang scribble/doc
@(require "mz.ss"
          (for-label racket/pretty
                     racket/gui/base))

@title{Init Libraries}

@defmodule*/no-declare[(racket/init)]{The @racketmodname[racket/init]
library is the default start-up library for Racket. It re-exports
the @racketmodname[racket], @racketmodname[racket/enter] and
@racketmodname[racket/help] libraries, and it sets
@racket[current-print] to use @racket[pretty-print].}

@defmodule*/no-declare[(racket/gui/init)]{The
@racketmodname[racket/gui/init] library is the default start-up
library for GRacket. It re-exports the @racketmodname[racket/init] and
@racketmodname[racket/gui/base] libraries, and it sets
@racket[current-load] to use @racket[text-editor-load-handler].}
