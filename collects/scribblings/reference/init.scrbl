#lang scribble/doc
@(require "mz.ss"
          (for-label racket/pretty
                     racket/gui/base))

@title{Init Libraries}

@defmodule*/no-declare[(racket/init)]{The @schememodname[racket/init]
library is the default start-up library for MzScheme. It re-exports
the @schememodname[scheme], @schememodname[racket/enter] and
@schememodname[racket/help] libraries, and it sets
@scheme[current-print] to use @scheme[pretty-print].}

@defmodule*/no-declare[(racket/gui/init)]{The
@schememodname[racket/gui/init] library is the default start-up
library for MrEd. It re-exports the @schememodname[racket/init] and
@schememodname[racket/gui/base] libraries, and it sets
@scheme[current-load] to use @scheme[text-editor-load-handler].}
