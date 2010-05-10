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

@defmodule*/no-declare[(racket/language-info)]{The
@racketmodname[racket/language-info] library provides a
@schemeidfont{get-info} function that takes any value and returns
another function; the returned function takes a key value and a
default value, and it returns @scheme['(#(racket/runtime-config
configure #f))] if the key is @scheme['configure-runtime] or the
default value otherwise.}

The vector @scheme['#(racket/language-info get-info #f)] is suitable
for attaching to a module as its language info to get the same
language information as the @scheme[racket/base] language.

@defmodule*/no-declare[(racket/runtime-config)]{The
@racketmodname[racket/runtime-config] library provides a
@racketidfont{configure} function that returns another function; the
returned function takes an value ans set @racket[print-as-expression]
to @scheme[#t].}

The vector @scheme[#(racket/runtime-config configure #f)] is suitable
as a member of a list of runtime-configuration specification (as
returned by a module's language-information function for the key
@racket['configure-runtime]) to obtain the same runtime configuration as
for the @schememodname[racket/base] language.
