#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/pretty
                     scheme/gui/base))

@title{Init Libraries}

@defmodule*/no-declare[(scheme/init)]{The @schememodname[scheme/init]
library is the default start-up library for MzScheme. It re-exports
the @schememodname[scheme] and @schememodname[scheme/help] libraries,
and it sets @scheme[current-print] to use @scheme[pretty-print].}

@defmodule*/no-declare[(scheme/gui/init)]{The
@schememodname[scheme/gui/init] library is the default start-up
library for MrEd. It re-exports the @schememodname[scheme/init] and
@schememodname[scheme/gui/base] libraries, and it sets
@scheme[current-load] to use @scheme[text-editor-load-handler].}
