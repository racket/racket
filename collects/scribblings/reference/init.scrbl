#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/pretty))

@title{Initialization}

@defmodule[scheme/init]{The @schememodname[scheme/init] library is the
default start-up library for @exec{mzscheme}. It re-exports the
@schememodname[scheme] and @schememodname[scheme/help] libraries, and
it sets @scheme[current-print] to use @scheme[pretty-print].}
